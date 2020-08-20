module Data.BinPacking.Item (module Data.BinPacking.Item) where

import Data.Monoid.Additive
import Prelude

import Control.MonadZero (guard)
import Data.Bounded (class Bounded, bottom)
import Data.Enum (class Enum, upFromIncluding)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericSucc, genericPred)
import Data.Ord (abs)
import Data.Array
import Data.Maybe
import Record (merge) as Record

type ItemRec r =
  (
    name :: String
  , flatDim :: Dimensions -- default unrotated dimensions
  , weight :: Int
  | r
  )

type Item = { | ItemRec () }

type ItemPositioned =
  {
    position :: Position
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
  {
      x :: Int
    , y :: Int
    , z :: Int
  }

type Bin =
  {
    name :: String
  , size :: Dimensions
  , maxWeight :: Int
  , items :: Array ItemPositioned
  , unfittedItems :: Array Item
  }

doesFit :: (Bounded Rotation) => Bin -> Item -> Position -> Maybe ItemPositioned
doesFit bin item pivot = do
  -- check if exceeds weight
  let (Additive binWeight) = foldMap (Additive <<< _.weight) bin.items
  guard $ bin.maxWeight > binWeight + item.weight -- Fail if exceeds weight
  -- check which (if any) rotation fits
  let itemFits = \rotItem ->
        bin.size.x >= pivot.x + rotItem.rotatedDim.x
        && bin.size.y >= pivot.y + rotItem.rotatedDim.y
        && bin.size.z >= pivot.z + rotItem.rotatedDim.z
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
        WidthAxis -> _.x
        HeightAxis -> _.y
        DepthAxis -> _.z


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

