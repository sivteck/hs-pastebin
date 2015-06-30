{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import Data.Time.Clock

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import qualified Data.Text as T

data Paste = Paste { content :: T.Text
                   } deriving Show

instance FromRow Paste where
    fromRow = Paste <$> field

instance ToRow Paste where
    toRow t = [toField (content t)]

-- Initialize database
createPasteBox :: Query
createPasteBox =
    [sql|
            CREATE TABLE IF NOT EXISTS paste (
                lid         SERIAL UNIQUE,
                created_at  TIMESTAMPTZ NOT NULL DEFAULT CURRENT_DATE,
                content     TEXT NOT NULL );
    |]

addPasteReturningID :: Connection -> Paste -> IO [Only Int]
addPasteReturningID con a = query con "INSERT INTO paste (created_at, content) VALUES (NOW (), ?) RETURNING lid" $ a

getPasteWithID :: Connection -> Int -> IO [Paste]
getPasteWithID c n = query c "SELECT TEXT(content) FROM paste WHERE lid = ?" $ Only n
