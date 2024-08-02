import Data.List
import System.Posix.Types
import qualified System.Posix.Files as Files
import System.FilePath.Find -- from filemanip, using `fold` from here
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

folding :: [FileInfo] -> FileInfo -> [FileInfo]
folding [] current = [current]
folding collection@(sample:_) current
  -- first we exclude files here because isDirectory' would not work
  | not (Files.isDirectory (infoStatus current)) = collection
  | metric current > metric sample               = [current]
  | metric current == metric sample              = current:collection
  | otherwise                                    = collection
  where metric = infoDepth

-- will eliminate hidden directories from recursion, but the `.` dir
visible :: FindClause Bool
visible = fileName /~? ".?*"

main = do
  skip <- getArgs
  mapM_ putStrLn . fmap infoPath =<< fold (filterPredicate skip) folding [] "."
