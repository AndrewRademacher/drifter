module Drifter.Types where

type Name        = String
type Description = String

data Change a = Change Name Description (Method a)

data Method a = Script FilePath
              | Function a

class Drifter a where
    migrate :: [Change a] -> IO (Either String ())
