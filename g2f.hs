import Data.PhotoSet.Gallery
import Data.PhotoSet
import System.IO

main = do
    cfg <- readFile "../../.g2f"
    let gurl = head (lines cfg)
    let gr = createGR gurl
    albums <- getAlbums gr
    mapM_ print albums

