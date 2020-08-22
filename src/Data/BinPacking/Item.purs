module Data.BinPacking.Item (module Data.BinPacking.Item) where

import Data.Array
import Data.Maybe
import Data.Monoid.Additive
import Prelude

import Control.MonadZero (class Bind, empty, guard)
import Data.Bounded (class Bounded, bottom)
import Data.Enum (class Enum, upFromIncluding)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericSucc, genericPred)
import Data.Ord (abs)
import Record (merge) as Record

type ItemRec r =
  ( name :: String
  , flatDim :: Dimensions -- default unrotated dimensions
  , weight :: Int
  | r
  )

type Item = { | ItemRec () }

type ItemPositioned =
  { position :: Position
  , rotatedDim :: Dimensions
  , rotation :: Rotation
  | ItemRec ()
  }

data Axis =
    WidthAxis
  | HeightAxis
  |  DepthAxis

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

type Bin = { positionedItems :: Array ItemPositioned
           | EmptyBinRec ()
           }


findFit :: (Bounded Rotation) => Bin -> Item -> Position -> Maybe ItemPositioned
findFit bin item pivot = do
  -- check if exceeds weight
  let (Additive binWeight) = foldMap (Additive <<< _.weight) bin.items
  guard $ bin.maxWeight > binWeight + item.weight -- Fail if exceeds weight
  -- check which (if any) rotation fits
  let itemFits = \rotItem ->
        bin.flatDim.x >= pivot.x + rotItem.rotatedDim.x
        && bin.flatDim.y >= pivot.y + rotItem.rotatedDim.y
        && bin.flatDim.z >= pivot.z + rotItem.rotatedDim.z
  let itemAllRotations = filter itemFits $ getAllRotationsForPositionedItem item pivot
  firstValidItemRotation itemAllRotations bin.items
  where
    firstValidItemRotation :: Array ItemPositioned -> Array ItemPositioned -> Maybe ItemPositioned
    firstValidItemRotation positionedItems binItems =
      case head positionedItems of
        Just posItem -> if any (itemsIntersect posItem) binItems then
                          firstValidItemRotation (drop 1 positionedItems) binItems
                        else
                          Just posItem 
        _ -> Nothing


itemsIntersect :: ItemPositioned -> ItemPositioned -> Boolean
itemsIntersect item1 item2 =
  intersectOn WidthAxis HeightAxis &&
  intersectOn WidthAxis DepthAxis &&
  intersectOn HeightAxis DepthAxis
  where
    intersectOn = rectIntersect item1 item2


rectIntersect :: ItemPositioned -> ItemPositioned -> Axis -> Axis -> Boolean
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


getAllRotationsForPositionedItem :: Item -> Position -> Array ItemPositioned
getAllRotationsForPositionedItem item pivot = do
  rotation <- upFromIncluding (bottom :: Rotation)
  pure $ Record.merge 
    { position: pivot
    , rotatedDim: rotateDim item.flatDim rotation
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

pack :: Array EmptyBin -> Array Item -> Tuple (Array Bin) (UnpackedItem) -- 
pack =
  let
    compareVolume = \dim1 dim2 -> compare 
                            (dim1.x * dim1.y * dim1.z)
                            (dim2.x * dim2.y * dim2.z)
    sortedBins = sortBy
                    (\bin1 bin2 -> compareVolume bin1.dim bin2.dim)
                    emptyBins
    sortedItems = sortBy
                    (\item1 item2 -> compareVolume item1.flatDim item2.flatDim)
                    items
    -- find bin that fits first item
  in
   
    -- if no bin fits item, add it to unpacked items
    -- if bin found, pack bin with all remaining items
    -- function returns packed bin and remaining items
    -- with rest of bins and items, repeat until no more items or bins
    -- use deleteAt 
    -- use findIndex
    -- use snoc (opposite of cons)
type Accum = { emptyBins :: Array EmptyBin
             , remainingItems :: Array Item
             , packedBins :: Array Bin
             , unPackableItems :: Array Item
             }
someFunc :: Accum -> Accum
someFunc accum =
  case head accum.remainingItems of
    -- Finished all items
    Nothing -> accum
    -- Process with first item
    Just firstItem ->
      case getBinForItem emptyBins firstItem of
        Just (Tuple3 firstPosItem emptyBin remainingBins) ->
          let
            Tuple packedBin remainingItems = packBin firstPos emptyBin (drop 1 accum.remainingItems)
          in
          -- Recursive call to process remaining items
          someFunc accum { emptyBins : remainingBins
                         , remainingItems : remainingItems
                         , packedBins : snoc accum.packedBins packedBin
                         }
        Nothing ->
          -- No bin was found for item.
          -- Add to unpackables and remove from further processing.
          someFunc accum { unPackableItems = snoc accum.unPackableItems firstItem
                         , remainingItems: drop 1 accum.remainingItems
                         }

getBinForItem emptyBins item =
  do
    binIndex <- findMap (\bin -> Tuple bin <$> findFit bin item {x: 0, y: 0, z: 0}) emptyBins
    emptyBin <- emptyBins !! indexBin
    remainingBins <- deleteAt indexBin emptyBins
    firstPosItem <- findFit emptyBin item {x: 0, y: 0, z: 0}
    Tuple3 firstPosItem emptyBin remainingBins

indexOfNextBin :: Item -> Array EmptyBin -> Maybe (Tuple EmptyBin ItemPositioned)
indexOfNextBin item emptyBins =
  

packBin :: Item -> EmptyBin -> Array Item -> Tuple Bin (Array Item)
packBin firstItem emptyBin items =
  let
    mFirstItemPost = findFit emptyBin item {x: 0, y: 0, z: 0}
  case mFirstItemPost of
    Nothing -> error "The item should have already had a valid placement"
    Just firstItemPos ->
      let
        firstStep = { bin: emptyBin
                    , positionedItems: singleton firstItemPos
                    , unfittedItems: []
                    }
        finalStep = foldl nextStep firstStep items
      in
        Tuple
          (Record.merge finalStep.bin {positionedItems: finalStep.positionedItems})
          finalStep.unfittedItems

type BinPackingStep = { bin :: EmptyBin
                      , positionedItems :: Array ItemPositioned
                      , unfittedItems :: Array Item -- items that didn't fit
                      }

nextStep :: BinPackingStep -> Item -> BinPackingStep
nextStep step currItem =
  case positionItem currItem of
    Nothing -> step {unfittedItems: snoc step.unfittedItems currItem}
    Just posItem -> step {positionedItems: snoc step.positionedItems posItem}
  where
    positionItem item = findMap
                          (\pivot -> findFit step.bin step.positionedItems pivot)
                          (generatePivots step.positionedItems)
    generatePivots = do
      axis <- upFromIncluding (bottom::Axis)
      binItem <- bin.items
      pure $ case axis of
        WidthAxis -> binItem.position {x: binItem.position.x + binItem.rotatedDim.x}
        HeightAxis -> binItem.position {y: binItem.position.y + binItem.rotatedDim.y}
        DepthAxis -> binItem.position {y: binItem.position.y + binItem.rotatedDim.y}

{-
Overview of algorithm (paraphrased/copied from paper, and modified to fit the current context)

--------- Bin Packing

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
