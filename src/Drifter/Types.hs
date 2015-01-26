{-# LANGUAGE TypeFamilies #-}

module Drifter.Types where

type Name        = String
type Description = Maybe String

data Change a = Change Name Description (Method a)

data family Method a

data family DBConnection a

class Drifter a where
    migrate :: DBConnection a -> [Change a] -> IO (Either String ())
