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
Other notes:
This algorithm is somewhat modified to fit functional setting.
As other implementations, it also considers weight limitations.
-}
module Data.BinPacking (module Re, module Data.BinPacking) where

import Data.BinPacking.BinPackingInternal (pack) as Re
import Data.BinPacking.Types (Bin, PackedBin, Item, PositionedItem, Packed) as Re

type PackedRep r = ( emptyBins :: Array Re.Bin
                    , packedBins :: Array Re.PackedBin
                    , unPackableItems :: Array Re.Item
                    | r
                    )

testBins :: Array Re.Bin
testBins = [ { name: "Small bin"
              , dim: {x: 10, y: 15, z: 20}
              , maxWeight: 100
              }
            , { name: "Medium Bin"
              , dim: {x: 100, y: 150, z: 200}
              , maxWeight: 1000
              }
            ]

testItems :: Array Re.Item
testItems = [ { name: "Item 1"
              , flatDim: {x: 2, y: 2, z: 1}
              , weight: 2
              }
            , { name: "Item 2"
              , flatDim: {x: 3, y: 3, z: 2}
              , weight: 3
              }
            ]

testPack = Re.pack testBins testItems
