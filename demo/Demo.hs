module Demo where

import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import           Lens.Micro
import           Lens.Micro.Platform
import qualified Data.Vector as V

---

get :: FilePath -> IO (Either Text VectorTile)
get fp = do
  mvt <- BS.readFile fp
  pure $ decode mvt >>= tile

-- | Read in raw protobuf data and decode it into a high-level type.
roads :: IO (Either Text VectorTile)
roads = get "test/roads.mvt"


onePolygon :: IO (Either Text VectorTile)
onePolygon = get "test/polygon.mvt"

layerNames :: Traversal' VectorTile Text
layerNames = layers . traverse . name

-- (\r -> sum $ r ^.. _Right . layers . traverse . polygons . traverse . geometries . to V.length) <$> roads

-- Find the first Polygon from the `water` layer.
-- (\r -> r ^? _Right . layers . ix "water" . polygons . _head . geometries . _head) <$> roads

-- POINTS: 76
-- LINESTRINGS: 576
-- POLYGONS: 555
