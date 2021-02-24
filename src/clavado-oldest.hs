
import qualified System.Posix.Files as Files
import qualified System.FilePath.Find as Find

import System.FilePath.Find ((/~?))
import System.Posix.Types
import System.Time.Utils

-- nofile is needed only as the first argument of @Find.fold@
data Evaluated = Evaluated { path:: FilePath, value:: EpochTime }
               | NoFile

instance Show Evaluated where
  show NoFile = "NoFile"
  show e      = (show (epochToClockTime (value e))) ++ " " ++ (path e)

instance Eq Evaluated where
  NoFile == NoFile = True
  NoFile == _      = False
  _      == NoFile = False
  e1     == e2     = (value e1) == (value e2)

-- nofiles are always discarded
instance Ord Evaluated where
  NoFile `compare` NoFile = LT
  _      `compare` NoFile = LT
  NoFile `compare` _      = GT
  e1     `compare` e2     = (value e1) `compare` (value e2)

type Metric = Find.FileInfo -> EpochTime

metric :: Metric
metric = Files.modificationTime . Find.infoStatus

folding :: Evaluated -> Find.FileInfo -> Evaluated
folding previous info
  | previous > evaluated = evaluated
  | otherwise            = previous
  where evaluated = Evaluated (Find.infoPath info) (metric info)

-- will eliminate hidden directories from recursion, but the `.` dir
visible :: Find.FindClause Bool
visible = Find.fileName /~? ".?*"

main :: IO Evaluated
main = do
  result <- Find.fold visible folding NoFile "."
  print result
  return result
