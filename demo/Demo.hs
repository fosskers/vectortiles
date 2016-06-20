module Demo where

import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import qualified Geography.VectorTile.Raw as R
import           Lens.Micro
import           Lens.Micro.Platform
import           System.Directory
import qualified Data.Vector as V

---

-- | Read in raw protobuf data and decode it into a high-level type.
roads :: IO (Either Text VectorTile)
roads = do
  mvt <- BS.readFile "test/roads.mvt"
  pure $ R.decode mvt >>= tile

layerNames :: Traversal' VectorTile Text
layerNames = layers . traverse . name

-- (\r -> sum $ r ^.. _Right . layers . traverse . polygons . traverse . geometries . to V.length) <$> roads
