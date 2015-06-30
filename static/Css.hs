{-# LANGUAGE OverloadedStrings #-}
module Css where

import Clay
import Clay.Background()
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TextIO (writeFile)

getCss :: FilePath -> IO ()
getCss basedir = TextIO.writeFile (basedir ++ "/css/style.css") generateCss

forma :: Css
forma = "form" ? do
    background green
    width (px 200)
    height (px 200)

generateCss :: T.Text
generateCss = render $ do
    forma
