# ❒ Simple Bin Packing Solver
                  _ _ _ _ _ _ _ _            _ _ _ _ _ _ _ _  
               /|                /|       /|                /|
              / |               / |      / |               / |
             /  |              /  |     /  |              /  |
            /   |             /   |    /   |             /   |
           /    |            /    |   /    |            /    |
          /     |           /     |  /     |           /     |
         /      | _ _ _ _ _/_ _ _ | /      | _ _ _ _ _/_ _ _ |
        /      /          /      / /      /          /      /
       /      /          /      / /      /          /      /
      /      /          /      / /      /          /      /
     /      /          /      / /      /          /      /
    / _ _ _/_ _ _ _ _        / / _ _ _/_ _ _ _ _        /
    |     /           |     /  |     /           |     /
    |    /            |    /   |    /            |    /
    |   / Bin         |   /    |   /             |   /
    |  /   Packing    |  /     |  /              |  /
    | /     Solver    | /      | /               | /
    |/                |/       |/                |/
    | _ _ _ _ _ _ _ _ |        | _ _ _ _ _ _ _ _ |
    

## Usage

```purescript
import Data.BinPacking

testBins :: Array Bin
testBins = [ { name: "Small bin"
              , dim: {x: 10, y: 15, z: 20}
              , maxWeight: 100
              }
            , { name: "Medium Bin"
              , dim: {x: 100, y: 150, z: 200}
              , maxWeight: 1000
              }
            ]

testItems :: Array Item
testItems = [ { name: "Item 1"
              , flatDim: {x: 2, y: 2, z: 1}
              , weight: 2
              }
            , { name: "Item 2"
              , flatDim: {x: 3, y: 3, z: 2}
              , weight: 3
              }
            ]

testPack :: Packed
testPack = pack testBins testItems
```


## Improvements

### Rewrite
This is an implementation of my understanding of an imperative algorithm. There might be better ways to express this problem in a more functional way. Starting with the fact that this uses awkward imperative logic, like deleting elements from the middle of lists. 

This was also written by a novice in functional programming. I would not call it "production ready" in any way, shape, or form. 

### Tests
The codebase could be improved to allow more testing of seperate functions.

I originally intended using quickcheck, but it involved getting around orphan instances. As of writing this, `purescript-jack` is not part of the package set this project uses, which seemed like an easier route.

More (or any) examples of correct packing could be used. Possibly generated with the help of a library written in another language.

### Reusability
I suspect there might be a simple way to make the types more polymorphic using open record types. In order to ensure the user that if they use an `Item UserItem` they will get `PositionedItem UserItem`.

`Bin` and `Item` records are augmented to `PackedBin` and `PositionedItem`, and not re-constructed. I hope this makes it easier to implement polymorphism once I understand how it's done.

### Expansion
This library currently uses only one heuristic.
It is based on the following paper and codebases:
* [Optimizing three-dimensional bin packing through simulation](erick_dube_507-034.pdf)
* https://github.com/bom-d-van/binpacking (Golang)
* https://github.com/gedex/bp3d (Golang)
* https://github.com/enzoruiz/3dbinpacking (Python)

Further heuristics and constraints could be considered.
For example, porting this 1D binpacking library https://hackage.haskell.org/package/Binpack or mixed integer programming solutions.

My notes on my understanding of the algorithm can be found in `src/Data/BinPacking.purs`.

### Performance
No idea. Haven't tested it.
