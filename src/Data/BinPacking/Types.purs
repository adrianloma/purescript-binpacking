module Data.BinPacking.Types (module Data.BinPacking.Types) where

import Prelude

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericSucc, genericPred)
import Data.Generic.Rep.Show (genericShow)


{-
Item Types
-}
type ItemRep r =
  ( name :: String
  , flatDim :: Dimensions -- default unrotated dimensions
  , weight :: Int
  | r
  )

type Item = { | ItemRep () }

type PositionedItem =
  { position :: Position
  , rotatedDim :: Dimensions
  , rotation :: Rotation
  | ItemRep ()
  }

{-
Bin Types
-}
type PackedBin = { positionedItems :: Array PositionedItem
                  | BinRep ()
                  }

type Bin = { | BinRep ()}


type BinRep r =
  ( name :: String
  , dim :: Dimensions
  , maxWeight :: Int
  | r
  )

{-
Packer types
-}
type Packed = { | PackedRep ()}

type PackedRep r = ( emptyBins :: Array Bin
                    , packedBins :: Array PackedBin
                    , unPackableItems :: Array Item
                    | r
                    )


{-
Intermediate step types
-}
type PackerStep = { pendingItems :: Array Item | PackedRep () }

type BinPackingStep = { bin :: Bin
                      , positionedItems :: Array PositionedItem
                      , unfittedItems :: Array Item
                      }
{-
ADTs and Coordinate types
-}


{-
Axis type and instances
-}
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

instance showAxis :: Show Axis where
  show = genericShow

{-
Rotation type and instances
-}
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

instance showRotation :: Show Rotation where
  show = genericShow

{-
Coordiante types
-}
type Dimensions = Coordinates
type Position = Coordinates
type Coordinates =
  { x :: Int
  , y :: Int
  , z :: Int
  }
