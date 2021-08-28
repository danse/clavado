\begin{code}

import System.Directory (
  listDirectory,
  getModificationTime,
  doesDirectoryExist)
import Data.List (sortOn)
import Data.Time (UTCTime)
import System.FilePath.Posix ((</>))
import Control.Exception (try)
import Control.Monad (when)
import System.Posix.Files (touchFile)
import System.Environment (getArgs)

\end{code}

@WithPrefix@ is used for a relative path that can include parent directories

\begin{code}

newtype WithPrefix = WithPrefix FilePath deriving Show
type FileInfo      = (WithPrefix, UTCTime)

\end{code}

Add information about a path

\begin{code}

addModification :: FilePath -> IO FileInfo
addModification p = do
  t <- getModificationTime p
  return (WithPrefix p, t)

isVisible :: FilePath -> Bool
isVisible p = head p /= '.'

\end{code}

We select the file in the current directory that was modified least recently

\begin{code}

headMay []    = Nothing
headMay (h:_) = Just h

latestModified :: Bool -> FileInfo -> IO (Maybe FileInfo)
latestModified verbose (WithPrefix path, _) = do
  when verbose $ putStrLn $ "visiting " <> path
  paths <- listDirectory path
  candidates <- mapM (addModification . (path </>)) (filter isVisible paths)
  return $ headMay $ sortOn snd candidates

doesInfoDirectoryExist :: FileInfo -> IO Bool
doesInfoDirectoryExist (WithPrefix path, _) = doesDirectoryExist path

\end{code}

We print the first ten lines of the file. If the file is shorter than ten lines we print a decorator to indicate its end

\begin{code}

filePreview :: FilePath -> UTCTime -> String -> String
filePreview path time = onLines (flip snoc (path <> " " <> show time) . t . f)
  where onLines f = unlines . f . lines
        f = filter (not . null) 
        t l = if length l < previewSize
            then (snoc l decorator)
            else take previewSize l
        snoc :: [a] -> a -> [a]
        snoc l e = l <> [e]
        previewSize :: Int
        previewSize = 10
        decorator :: String
        decorator = replicate 80 '_'

\end{code}

@iterateMWhile@ returns the first value which does not satisfy the predicate, applying the iterator when the predicate is satisfied

\begin{code}

iterateWhileExists :: (FileInfo -> IO (Maybe FileInfo)) -> FileInfo -> IO FileInfo
iterateWhileExists iterating initial = do
  pass <- doesInfoDirectoryExist initial
  if pass
    then do
      maybeNext <- iterating initial
      case maybeNext of
        Nothing -> return initial
        Just next -> iterateWhileExists iterating next
    else return initial

main :: IO ()
main = do
  args <- getArgs
  let verbose = not $ null args
  a <- getModificationTime "."
  let start = (WithPrefix ".", a)
  (WithPrefix path, time) <- iterateWhileExists (latestModified verbose) start
  eitherContents <- try (readFile path) :: IO (Either IOError String)
  let found contents = do
        touchFile path
        putStrLn $ filePreview path time contents
  either print found eitherContents

\end{code}
