import Data.List
import System.Posix.Types
import qualified System.Posix.Files as Files
import System.FilePath.Find
import System.Environment
import Control.Monad
import Data.Ord
import Control.Applicative ((<$>))

-- isDirectory' :: FindClause Bool
-- isDirectory' = (== Directory) <$> fileType

filterPredicate :: [String] -> FindClause Bool
filterPredicate skip = visible &&? notSkip skip -- &&? isDirectory' -- this does not filter as expected
  where notSkip [] = always
        notSkip (a:b) = (fileName /~? a ) &&? notSkip b

folding :: Maybe FileInfo -> FileInfo -> Maybe FileInfo
folding Nothing current = Just current
folding (Just best) current
  | metric best > metric current                 = Just best
  | not (Files.isDirectory (infoStatus current)) = Just best -- because isDirectory' would not work
  | otherwise                                    = Just current
  where metric = infoDepth

-- will eliminate hidden directories from recursion, but the `.` dir
visible :: FindClause Bool
visible = fileName /~? ".?*"

main = do
  skip <- getArgs
  result <- fold (filterPredicate skip) folding Nothing "."
  case result of
    Just info -> putStrLn (infoPath info)
    Nothing -> putStrLn "."
  
