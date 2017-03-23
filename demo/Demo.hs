module Demo where

import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import qualified Data.Vector as V
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import           Lens.Micro
import           Lens.Micro.Platform ()
import           Text.Pretty.Simple

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

clearLake :: IO (Either Text VectorTile)
clearLake  = get "test/clearlake.mvt"

-- | A small section whose upper left corner should store part of the lake.
cl38 :: IO (Either Text VectorTile)
cl38 = get "test/16.14569.22038.mvt"

-- | Mostly lake, with the beach in the bottom right.
cl37 :: IO (Either Text VectorTile)
cl37 = get "test/16.14569.22037.mvt"

-- | Just lake.
cl36 :: IO (Either Text VectorTile)
cl36 = get "test/16.14569.22036.mvt"

tiny :: IO (Either Text VectorTile)
tiny = get "test/16.14508.21533.mvt"

diomede :: IO (Either Text VectorTile)
diomede = get "test/511.mvt"

layerNames :: Traversal' VectorTile Text
layerNames = layers . traverse . name

geomCounts :: VectorTile -> (Int, Int, Int)
geomCounts vt = (pnt, lns, ply)
  where pnt = sum $ vt ^.. layers . traverse . points . traverse . geometries . to V.length
        lns = sum $ vt ^.. layers . traverse . linestrings . traverse . geometries . to V.length
        ply = sum $ vt ^.. layers . traverse . polygons . traverse . geometries . to V.length

metas :: VectorTile -> [(Int, M.Map Text Val)]
metas vt = zip fs ms
  where fs = vt ^.. layers . traverse . polygons . traverse . featureId
        ms = vt ^.. layers . traverse . polygons . traverse . metadata

lake :: Int -> VectorTile -> [V.Vector Polygon]
lake n vt = vt ^.. layers . traverse . polygons . traverse . filtered (\(Feature i _ _) -> i == n) . geometries

zing = do
  cl38 >>= print . (\vt -> vt ^?! _Right . to (lake 2) . _head . _head)
  cl37 >>= print . (\vt -> vt ^?! _Right . to (lake 2) . _head . _head)
  cl36 >>= print . (\vt -> vt ^?! _Right . to (lake 2) . _head . _head)

fling :: IO ()
fling = tiny >>= pPrint

-- (\r -> sum $ r ^.. _Right . layers . traverse . polygons . traverse . geometries . to V.length) <$> roads

-- Find the first Polygon from the `water` layer.
-- (\r -> r ^? _Right . layers . ix "water" . polygons . _head . geometries . _head) <$> roads

-- POINTS: 76
-- LINESTRINGS: 576
-- POLYGONS: 555
