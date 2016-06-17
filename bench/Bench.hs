module Main where

import           Control.Monad ((>=>))
import           Criterion.Main
import qualified Data.ByteString as BS
import           Geography.VectorTile
import qualified Geography.VectorTile.Raw as R

---

main :: IO ()
main = do
  mvt <- BS.readFile "test/roads.mvt"
  defaultMain [ bgroup "Decoding"
                [ bench "Raw.VectorTile" $ nf R.decode mvt
                , bench "VectorTile" $ nf (R.decode >=> tile) mvt
                ]
              ]
