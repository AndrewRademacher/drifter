{-# LANGUAGE CPP #-}
module Drifter.Graph
    ( resolveDependencyOrder
    , Drifter(..)
    , migrate
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative  as A
import           Control.Monad
import           Data.Graph.Inductive (Edge, Gr, UEdge, mkGraph, topsort')
import qualified Data.Map.Strict      as Map
import           Data.Maybe
-------------------------------------------------------------------------------
import           Drifter.Types
-------------------------------------------------------------------------------


labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(a, b) -> (a, b, ()))

-- | Take an unordered list of changes and put them in dependency
-- order. 'migrate' will do this automatically.
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


class Drifter a where
    -- | How to run a single, isolated migration.
    migrateSingle :: DBConnection a -> Change a -> IO (Either String ())


-- | Runs a list of changes. They will automatically be sorted and run
-- in dependency order. Will terminate early on error.
migrate :: Drifter a => DBConnection a -> [Change a] -> IO (Either String ())
migrate conn csUnsorted = runEitherT (mapM_ go cs)
  where cs = resolveDependencyOrder csUnsorted
        go = EitherT . migrateSingle conn


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }


instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}

instance Monad m => A.Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}

#if __GLASGOW_HASKELL__ < 800
  fail = EitherT . fail
  {-# INLINE fail #-}
#endif
