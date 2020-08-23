module Data.BinPacking.BinPackingInternal (pack) where

import Data.BinPacking.Types (Axis(..), Bin, BinPackingStep, Item, Packed, PackerStep, Position, PositionedItem, Rotation(..))

import Data.Array (all, deleteAt, drop, find, findIndex, findMap, foldl, head, singleton, snoc, sortBy, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Prelude
import Data.Symbol (SProxy(..))

import Control.MonadZero (guard)
import Data.Enum (upFromIncluding)
import Data.Foldable (foldMap)
import Data.Ord (abs)
import Record (merge, delete) as Record


pack :: Array Bin -> Array Item -> Packed
pack emptyBins itemsToPack =
  let
    compareVolume = \dim1 dim2 -> compare
                            (dim1.x * dim1.y * dim1.z)
                            (dim2.x * dim2.y * dim2.z)
    sortedBins = sortBy
                    (\bin1 bin2 -> compareVolume bin1.dim bin2.dim)
                    emptyBins
    sortedItems = sortBy
                    (\item1 item2 -> compareVolume item1.flatDim item2.flatDim)
                    itemsToPack
    firstPackerStep = { emptyBins: sortedBins
                      , pendingItems: sortedItems
                      , packedBins: []
                      , unPackableItems: []
                      }
    lastPackerStep = getFinalPackerStep firstPackerStep
  in
   Record.delete (SProxy :: SProxy "pendingItems") lastPackerStep

getFinalPackerStep :: PackerStep -> PackerStep
getFinalPackerStep  step@{pendingItems: []} = step
getFinalPackerStep step =
  case tryPackNextBin of
    Just newStep -> getFinalPackerStep newStep
    Nothing ->
      -- No bin was found for item.
      -- Add to unpackables and remove from further processing.
      getFinalPackerStep step { unPackableItems= fromMaybe
                                    step.unPackableItems
                                    (snoc step.unPackableItems <$> head step.pendingItems)
                              , pendingItems= drop 1 step.pendingItems
                              }
  where
    tryPackNextBin =
      do
        firstItem <- head step.pendingItems
        pendingItems <- Just $ drop 1 step.pendingItems
        binIndex <- findIndex (\bin -> isJust $ findFit bin firstItem [] {x: 0, y: 0, z: 0}) step.emptyBins
        emptyBin <- step.emptyBins !! binIndex
        remainingBins <- deleteAt binIndex step.emptyBins
        firstPosItem <- findFit emptyBin firstItem [] {x: 0, y: 0, z: 0}
        let firstStep = { bin: emptyBin
                        , positionedItems: singleton firstPosItem
                        , unfittedItems: []
                        }
        let finalStep = foldl addItemToBin firstStep pendingItems
        let packedBin = Record.merge finalStep.bin {positionedItems: finalStep.positionedItems}
        pure $ step { emptyBins= remainingBins
                     , pendingItems= finalStep.unfittedItems
                     , packedBins= snoc step.packedBins packedBin
                     }

findFit :: Bin -> Item -> Array PositionedItem -> Position -> Maybe PositionedItem
findFit bin item positionedItems pivot = do
  -- check if exceeds weight
  let (Additive binWeight) = foldMap (Additive <<< _.weight) positionedItems
  guard $ bin.maxWeight > binWeight + item.weight -- Fail if exceeds weight
  let itemAllRotations = makeRotationsForItemInBinWithPivot item bin pivot
  -- Find first rotation that doesn't intersect with any binItem.
  find
    (\rotatedItem -> all (not $ itemsIntersect rotatedItem) positionedItems)
    itemAllRotations


makeRotationsForItemInBinWithPivot :: Item -> Bin -> Position -> Array PositionedItem
makeRotationsForItemInBinWithPivot item bin pivot = do
  rotation <- upFromIncluding (bottom :: Rotation)
  let rotatedDim = rotateDim item.flatDim rotation
  guard $ -- Check if rotated item fits in bin.
        bin.dim.x >= pivot.x + rotatedDim.x
        && bin.dim.y >= pivot.y + rotatedDim.y
        && bin.dim.z >= pivot.z + rotatedDim.z
  pure $ Record.merge
    { position: pivot
    , rotatedDim: rotatedDim
    , rotation: rotation
    } item
  where
    rotateDim dim rotation =
      case rotation of
        XYZ -> {x: dim.x, y: dim.y, z: dim.z}
        YXZ -> {x: dim.y, y: dim.x, z: dim.z}
        YZX -> {x: dim.y, y: dim.z, z: dim.x}
        ZYX -> {x: dim.z, y: dim.y, z: dim.x}
        ZXY -> {x: dim.z, y: dim.x, z: dim.y}
        XZY -> {x: dim.x, y: dim.z, z: dim.y}

addItemToBin :: BinPackingStep -> Item -> BinPackingStep
addItemToBin step currItem =
  case findPositionFor currItem of
    Nothing -> step {unfittedItems= snoc step.unfittedItems currItem}
    Just posItem -> step {positionedItems= snoc step.positionedItems posItem}
  where
    findPositionFor item = findMap
                          (\pivot -> findFit step.bin currItem step.positionedItems pivot)
                          (generatePivots step.positionedItems)
    generatePivots binItems = do
      axis <- upFromIncluding (bottom::Axis)
      binItem <- binItems
      pure $ case axis of
        WidthAxis -> binItem.position {x = binItem.position.x + binItem.rotatedDim.x}
        HeightAxis -> binItem.position {y = binItem.position.y + binItem.rotatedDim.y}
        DepthAxis -> binItem.position {z = binItem.position.z + binItem.rotatedDim.z}


itemsIntersect :: PositionedItem -> PositionedItem -> Boolean
itemsIntersect item1 item2 =
  intersectOn WidthAxis HeightAxis &&
  intersectOn WidthAxis DepthAxis &&
  intersectOn HeightAxis DepthAxis
  where
    intersectOn = rectIntersect item1 item2


rectIntersect :: PositionedItem -> PositionedItem -> Axis -> Axis -> Boolean
rectIntersect item1 item2 axisX axisY =
  let
    getX = makeGetter axisX
    getY = makeGetter axisY
    iCalc = \getField -> abs (cval item1 getField - cval item2 getField)
    ix = iCalc getX
    iy = iCalc getY
    dx = (getX item1.rotatedDim + getX item2.rotatedDim) / 2
    dy = (getY item1.rotatedDim + getY item2.rotatedDim) / 2
  in
    ix < dx && iy < dy
  where
    cval item getField = (getField item.position + getField item.rotatedDim / 2)
    makeGetter axis =
      case axis of
        WidthAxis  -> _.x
        HeightAxis -> _.y
        DepthAxis  -> _.z
