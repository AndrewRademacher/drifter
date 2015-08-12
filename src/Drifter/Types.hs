{-# LANGUAGE TypeFamilies #-}

module Drifter.Types where

import           Data.Text

type Name        = Text
type Description = Text

data Change a = Change
        { changeName         :: Name
        , changeDescription  :: Maybe Description
        , changeDependencies :: [Name]
        , changeMethod       :: Method a
        }

data family Method a

data family DBConnection a
