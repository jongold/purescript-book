module FileOperations where

import Prelude
import Data.Array (concatMap, (:))
import Data.Path (Path, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Write a function onlyFiles which returns all files (not directories)
-- in all subdirectories of a directory.
onlyFiles :: Path -> Array Path
onlyFiles root = concatMap (onlyFiles) (ls root)

-- TODO: Write a fold to determine the largest and smallest files in the filesystem


-- TODO: Write a function whereIs to search for a file by name. The function
-- should return a value of type Maybe Path, indicating the directory
-- containing the file, if it exists. It should behave as follows:
--  > whereIs "/bin/ls"
--  Just (/bin/)
--
--  > whereIs "/bin/cat"
--  Nothing
