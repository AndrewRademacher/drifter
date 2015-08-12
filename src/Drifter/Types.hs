{-# LANGUAGE TypeFamilies #-}

module Drifter.Types where

import           Data.Text

newtype ChangeName = ChangeName
        { changeNameText :: Text } deriving (Show, Eq, Ord)
type Description = Text

data Change a = Change
        { changeName         :: ChangeName
        , changeDescription  :: Maybe Description
        , changeDependencies :: [ChangeName]
        , changeMethod       :: Method a
        }

data family Method a

data family DBConnection a
