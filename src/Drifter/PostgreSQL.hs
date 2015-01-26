module Drifter.PostgreSQL where

import           Database.PostgreSQL.Simple

import           Drifter.Types

data Postgres = Postgres
        { conn :: Connection
        }

instance Drifter Postgres where
    migrate = undefined
