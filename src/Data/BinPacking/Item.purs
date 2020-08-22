module Data.BinPacking.Item (module Data.BinPacking.Item) where

import Data.BinPacking.Types

import Data.Array (all, deleteAt, drop, find, findIndex, findMap, foldl, head, singleton, snoc, sortBy, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Prelude
import Data.Symbol (SProxy(..))

import Control.MonadZero (guard)
import Data.Enum (class Enum, upFromIncluding)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericSucc, genericPred)
import Data.Ord (abs)
import Record (merge, delete) as Record

type ItemRec r =
  ( name :: String
  , flatDim :: Dimensions -- default unrotated dimensions
  , weight :: Int
  | r
  )


type Item = { | ItemRec () }

type PositionedItem =
  { position :: Position
  , rotatedDim :: Dimensions
  , rotation :: Rotation
  | ItemRec ()
  }

data Axis =
    WidthAxis
  | HeightAxis
  |  DepthAxis

derive instance eqAxis :: Eq Axis
derive instance ordAxis :: Ord Axis
derive instance genericAxis :: Generic Axis _

instance boundedAxis :: Bounded Axis where
  top = genericTop
  bottom = genericBottom

instance enumAxis :: Enum Axis where
  succ = genericSucc
  pred = genericPred

data Rotation =
    XYZ --   WHD
  | YXZ -- | HWD
  | YZX -- | HDW
  | ZYX -- | DHW
  | ZXY -- | DWH
  | XZY -- | WDH

derive instance eqRotation :: Eq Rotation
derive instance ordRotation :: Ord Rotation
derive instance genericRotation :: Generic Rotation _

instance boundedRotation :: Bounded Rotation where
  top = genericTop
  bottom = genericBottom

instance enumRotation :: Enum Rotation where
  succ = genericSucc
  pred = genericPred

type Dimensions = Coordinates
type Position = Coordinates
type Coordinates =
  { x :: Int
  , y :: Int
  , z :: Int
  }

type EmptyBinRec r =
  ( name :: String
  , dim :: Dimensions
  , maxWeight :: Int
  | r
  )

type EmptyBin = { | EmptyBinRec ()}

type PackedBin = { positionedItems :: Array PositionedItem
           | EmptyBinRec ()
           }

findFit :: (Bounded Rotation) => EmptyBin -> Item -> Array PositionedItem -> Position -> Maybe PositionedItem
findFit bin item positionedItems pivot = do
  -- check if exceeds weight
  let (Additive binWeight) = foldMap (Additive <<< _.weight) positionedItems
  guard $ bin.maxWeight > binWeight + item.weight -- Fail if exceeds weight
  let itemAllRotations = makeRotationsForItemInBinWithPivot item bin pivot
  -- Find first rotation that doesn't intersect with any binItem.
  find
    (\rotatedItem -> all (not $ itemsIntersect rotatedItem) positionedItems)
    itemAllRotations


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


makeRotationsForItemInBinWithPivot :: Item -> EmptyBin -> Position -> Array PositionedItem
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

pack :: Array EmptyBin -> Array Item -> Packed
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

type PackedRep r = ( emptyBins :: Array EmptyBin
                       , packedBins :: Array PackedBin
                       , unPackableItems :: Array Item
                       | r
                       )

type Packed = { | PackedRep ()}

type PackerStep = { pendingItems :: Array Item | PackedRep () }

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
        let finalStep = foldl nextStep firstStep pendingItems
        let packedBin = Record.merge finalStep.bin {positionedItems: finalStep.positionedItems}
        pure $ step { emptyBins= remainingBins
                     , pendingItems= finalStep.unfittedItems
                     , packedBins= snoc step.packedBins packedBin
                     }

type BinPackingStep = { bin :: EmptyBin
                      , positionedItems :: Array PositionedItem
                      , unfittedItems :: Array Item -- items that didn't fit
                      }

nextStep :: BinPackingStep -> Item -> BinPackingStep
nextStep step currItem =
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
{-
Overview of algorithm (paraphrased/copied from paper, and modified to fit the current context)

--------- PackedBin Packing

1. Background

Bin packing is an NP-Hard problem. Solution is a heuristic based approach.
The running time for Best Fit is O(n(log n)) and for First Fit Decreasing it  is O(n(log n)) excluding the running time for sorting.

We have three directions in which to pack the items (x, y, z). Each Item has six rotation types. Consider an item: The six rotation types can be obtained by rotating about the x, y and/or z axis as shown in Table 1: The bins are packed one at a time and the algorithms use a series of pivot points at which to pack the item.

Table 1:

*-------------------------------------------*
| Rotation | First axis to  | Second axis to|
| type     | rotate about   | rotate about  |
|----------+----------------+---------------|
|        0 | -              | -             |
|        1 | Z              | -             |
|        2 | Y              | -             |
|        3 | X              | Y             |
|        4 | X              | -             |
|        5 | X              | Z             |
*-------------------------------------------*

This is represented with the `Rotation` type which has a cardinality of 6.

2. Algorithm
Decide on a packing direction. Each bin has three directions in which to pack, a width (or x) direction, a height (or y) direction, a depth  (or z) direction.

Pack one bin at a time. We first choose a pivot point.  The pivot is an (x, y, z) coordinate which represents a point in a particular 3D bin at which an attempt  to pack  an item will be made. The back lower left  corner  of the  item will be placed at  the pivot. If the item cannot be packed at the pivot  position then it is rotated until it can be packed at the pivot point or until we have tried all 6 possible rotation types. If after rotating  it, the item still cannotbe packed at the pivot point, then we move on to packing another item and add the unpacked item to a list of items that will be packed after an attempt to pack the remaining items is made. The first pivot in an empty bin is always (0,0,0).

1. Choose smallest axis of bin, that will be the "pack by" dimension. (not sure what this is used for, see step 5.1.1.1)
   Set notPacked to the full list of items
-- Start loop
1. toPack = notPacked, notPacked = {}
2. Get new empty bin.
3. Set pivot to (0,0,0)
4. For the first item to pack, try fitting, rotate until it does. Pack.
5. For each item to pack (except the first) 
5.1 For each axis
5.1.1 For each packed item in the bin
5.1.1.1 Choose a new pivot using the current packed item and the current axis
This part I didn't fully understand in the paper.
In several implementations of this algorithm, this part is implemented as iterating each axis. Starting with the width axis. This is what is done here as well.
pivot = case axis of
            x -> binItem.position + width (back lower right)
            y -> binItem.position + height (back upper left)
            z -> binItem.position + depth (front lower left)

                           +--------------+
       back upper left    /|             /|
  +(0,bi.height,0) \     / |            / |
                    +-> *--+-----------*  |
                        |  |           |  |
       front lower left |  |           |  |
  +(0,0,bi.depth)    \  |  |           |  |
                      +-|->+-----------+--+
                        | /            | /
                        |/             |/
 (bi.x, bi.y, bi.z) --> *--------------*  <-- back lower right
                                                 +(bi.width,0,0)

5.1.1.2 Rotate around the 6 rotation types until item fits.
5.1.1.3 If it fits, pack item. Else, add to list of unpacked items.
6. Go to step 1
-----
Other notes.
This algorithm is somewhat modified to fit functional setting.
As other implementations, it also considers weight limitations.
-}
