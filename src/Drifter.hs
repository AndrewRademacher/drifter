module Drifter
    (
    -- * Managing Migrations
      resolveDependencyOrder
    , changeSequence
    , migrate
    -- * Types
    , Drifter(..)
    , ChangeName(..)
    , Change(..)
    , Description
    , Method
    , DBConnection
    ) where


-------------------------------------------------------------------------------
import           Data.List
-------------------------------------------------------------------------------
import           Drifter.Graph
import           Drifter.Types
-------------------------------------------------------------------------------


-- | This is a helper for the common case of where you just want
-- dependencies to run in list order. This will take the input list
-- and set their dependencies to run in the given sequence.
changeSequence :: [Change a] -> [Change a]
changeSequence [] = []
changeSequence (x:xs) = reverse $ snd $ foldl' go (x, [x]) xs
  where
    go :: (Change a, [Change a]) -> Change a -> (Change a, [Change a])
    go (lastChange, xs') c =
      let c' = c { changeDependencies = [changeName lastChange] }
      in (c', c':xs')
