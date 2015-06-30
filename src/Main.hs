{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Model 

import Lucid
import Lucid.Base

import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Database.PostgreSQL.Simple

import Data.Monoid
import Control.Monad.Trans

import Data.Text.Lazy (toStrict)
import Data.Text (pack, Text)

-- helper function
renderLucid :: Html () -> ActionT IO a
renderLucid = html . toStrict . renderText

-- template
temp :: Monad m => HtmlT m () -> HtmlT m ()
temp content = do
    doctype_
    html_ $ do
        head_ [] $ do
            title_ [] "A dead simple Pastebin"
            link_ [rel_ "stylesheet", href_ "/css/style.css"]
        body_ [] $ do
            div_ [] $ do
                h1_ [] "Stores pastes in postgresql database"
                h2_ [] "sends POST request to /la"
            div_ [] $ do
                p_ [] "/:Int is used to access pastes eg. /42"
                content

pasteArea :: Monad m => HtmlT m ()
pasteArea = temp $ form_ [name_ "pastebox", method_ "POST", action_ "/la"] (do 
                    textarea_ [cols_ "100", rows_ "30",name_ "q"] ""
                    input_ [type_ "submit"])

---------------------------------------------------------

pasteIt :: Connection -> Text -> ActionT IO ()
pasteIt c p = do
    [Only x] <- lift $ addPasteReturningID c (Paste p)
    redirect (pack ("/" ++ (show  x)))


retrieveIt :: Connection -> Int -> ActionT IO ()
retrieveIt c i = do
    [Paste txt] <- lift $ getPasteWithID c i
    text txt

pasteR :: Path '[]
pasteR = "la"

retrieveR :: Path '[Int]
retrieveR = var

main :: IO ()
main = do
    conn <- connectPostgreSQL ""
    runSpock 8080 $ spockT id $ do
        middleware (staticPolicy (addBase "static"))
        get root $
            renderLucid pasteArea 

        post pasteR $ do
            paste <- param "q"
            case paste of
                Just t -> pasteIt conn t
                Nothing -> liftIO $ print "lulz"

        get retrieveR $ \a ->
            retrieveIt conn a
