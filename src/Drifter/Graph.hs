module Drifter.Graph
    ( resolveDependencyOrder
    ) where

import           Control.Applicative
import           Data.Graph.Inductive (Edge, Gr, UEdge, mkGraph, topsort')
import qualified Data.Map.Strict      as Map
import           Data.Maybe

import           Drifter.Types

labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(a, b) -> (a, b, ()))

resolveDependencyOrder :: [Change a] -> [Change a]
resolveDependencyOrder cs = topsort' $ graphDependencies cs

graphDependencies :: [Change a] -> Gr (Change a) ()
graphDependencies cs = mkGraph nodes (labUEdges edges)
    where nodes = zip [1..] cs
          nMap  = Map.fromList $ map (\(i, c) -> (changeName c, i)) nodes
          edges = catMaybes
                $ map (\(a, b) -> (,) <$> a <*> b)
                $ concat
                $ map (\c -> map (\dn -> ( Map.lookup dn nMap
                                         , Map.lookup (changeName c) nMap))
                                 (changeDependencies c))
                      cs
