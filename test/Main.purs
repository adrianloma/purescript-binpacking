module Test.Main where

import Prelude
import Data.Maybe

import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class.Console (log, logShow)

import Data.Time.Duration (Milliseconds(..))
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.BinPacking.BinPackingInternal 
import Data.BinPacking.Types

import Data.Array
import Data.Foldable hiding (length)

import Type.Proxy (Proxy(..), Proxy2(..))

main :: Effect Unit
main = do
  log "📦 Running Bin Packing Tests 📦"
  launchAff_ $ runSpec [consoleReporter] do
    describe "Bin packing general algorithm" do
        describe "Packer" do
          compareSolutions
        describe "Features" do
          it "runs in NodeJS" $ pure unit
          it "runs in the browser" $ pure unit
          it "supports streaming reporters" $ pure unit



compareSolutions ::  Spec Unit
compareSolutions = do
  let smallBin = { name: "Small bin"
                  , dim: {x: 10, y: 15, z: 20}
                  , maxWeight: 100
                  }
  let mediumBin = { name: "Medium Bin"
                  , dim: {x: 100, y: 150, z: 200}
                  , maxWeight: 1000
                  }
  describe "Behavior with no items" do
    it "returns first empty bin when there are no items to pack" do
      let packer = pack [smallBin] []
      head packer.emptyBins `shouldEqual` Just smallBin
    it "returns second empty bin when there are no items to pack" do
      let packer = pack [mediumBin] []
      head packer.emptyBins `shouldEqual` Just mediumBin
    it "returns both empty bins (in increasing volume size) when there are no items to pack" do
      let packer = pack [smallBin, mediumBin] [] -- note that order matters here.
      packer.emptyBins `shouldEqual` [smallBin, mediumBin]
  describe "Behavior with item sizes" do
    let smallItem = { name: "Item 1"
                    , flatDim: {x: 2, y: 2, z: 1}
                    , weight: 2
                    }
    let mediumItem = { name: "Item 2"
                     , flatDim: {x: 5, y: 3, z: 2}
                     , weight: 3
                     }
    describe "Optimal fit scenario" do
      let currItem = mediumItem {weight= 0}
      let currBin = smallBin
      let itemsToRepeat = (currBin.dim.x / currItem.flatDim.x) *
                            (currBin.dim.y / currItem.flatDim.y) *
                            (currBin.dim.z / currItem.flatDim.z)
      let packer = pack [smallBin, mediumBin] (replicate itemsToRepeat currItem)
      it "Contains only one packed box" do
        length packer.packedBins `shouldEqual` 1
      it "Packed bin contains all items" do
        (length <$> _.positionedItems <$> head packer.packedBins) `shouldEqual` Just itemsToRepeat
      it "Packed items have the same volume as original" do
        let sumVolume = \accum {x: x, y: y, z: z} -> accum + (x * y * z)
        let mPackedBin = head packer.packedBins
        let mPositionedItems = _.positionedItems <$> mPackedBin
        let mFlatDims = map _.flatDim <$> mPositionedItems
        let mTotalVolume = foldl sumVolume 0 <$> mFlatDims
        mTotalVolume
            `shouldEqual`
            (Just $ foldl sumVolume 0 $ map _.flatDim (replicate itemsToRepeat currItem))
      it "Bin is fully packed (no remaining volume)" do
        let sumVolume = \accum {x: x, y: y, z: z} -> accum + (x * y * z)
        let mPackedBin = head packer.packedBins
        let mBinDim = _.dim <$> mPackedBin
        let mTotalVolume = sumVolume 0 <$> mBinDim
        mTotalVolume
            `shouldEqual`
            (Just $ foldl sumVolume 0 $ map _.flatDim (replicate itemsToRepeat currItem))
    describe "Overflow from optimal scenario" do
      let currItem = mediumItem {weight= 0}
      let currBin = smallBin
      let itemsToRepeat = (currBin.dim.x / currItem.flatDim.x) *
                            (currBin.dim.y / currItem.flatDim.y) *
                            (currBin.dim.z / currItem.flatDim.z) + 1
      let packer = pack [smallBin, mediumBin] (replicate itemsToRepeat currItem)
      it "Contains two packed boxes" do
        length packer.packedBins `shouldEqual` 2
      it "First bin should contain all but one item" do
        (length <$> _.positionedItems <$> head packer.packedBins) `shouldEqual` Just (itemsToRepeat - 1)
      it "Packed items have the same volume as original" do
        let sumVolume = \accum {x: x, y: y, z: z} -> accum + (x * y * z)
        let mPositionedItems = join $ _.positionedItems <$> packer.packedBins
        let mFlatDims = _.flatDim <$> mPositionedItems
        let mTotalVolume = foldl sumVolume 0 mFlatDims
        mTotalVolume
            `shouldEqual`
            (foldl sumVolume 0 $ map _.flatDim (replicate itemsToRepeat currItem))
      it "Total volume should exceed small bin volume" do
        let sumVolume = \accum {x: x, y: y, z: z} -> accum + (x * y * z)
        let mPackedBin = head packer.packedBins
        let mBinDim = _.dim <$> mPackedBin
        let mTotalVolume = sumVolume 0 <$> mBinDim
        mTotalVolume
            `shouldNotEqual`
            (Just $ foldl sumVolume 0 $ map _.flatDim (replicate itemsToRepeat currItem))
