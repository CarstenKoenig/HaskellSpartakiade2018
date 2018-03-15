{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Formel
import           Lucid
import qualified Lucid.Html5 as H
import           Network.Wai (Middleware)
import           Network.Wai.Middleware.Static (staticPolicy, addBase, noDots)
import           System.Environment (lookupEnv)
import           Web.Scotty


main :: IO ()
main = do
  port <- maybe 80 read <$> lookupEnv "PORT"

  scotty port $ do
    -- serve static files form "./static" folder
    middleware serveStaticFiles
    -- post formulas as string in json-body that will get evaluated and sent back as a json-number
    -- try it: curl -H "Content-Type: application/json" -X POST -d '"3 + 3 * 5 / 30 - 22.22"' http://localhost:3000/eval
    post "/eval" $ do
      formel <- jsonData
      liftIO $ print formel
      maybe (raise "konnte Formel nicht parsen") (json . eval) $ (parse formel :: Maybe (Formel Double))
    get "" $ do
      html $ renderText (page Nothing)
    post "" $ do
      formel <- param "formel"
      let result = eval <$> parse formel
      html $ renderText (page $ (formel,) <$>  result)


serveStaticFiles :: Middleware
serveStaticFiles = staticPolicy (noDots <> addBase "static")


page :: Maybe (String, Double) -> Html ()
page result = H.doctypehtml_ $ do
  H.head_ $ do
    H.meta_ [ H.charset_ "UTF-8" ]
    H.meta_ [ H.name_ "viewport", H.content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
    H.link_ [ H.rel_ "stylesheet", H.href_ "bootstrap.css" ]

    H.title_  "Rechner"

  H.body_ $ do
    H.div_ [ H.class_ "content" ] $ do
      caption
      inputForm

    script "jquery.js"
    script "pooper.js"
    script "bootstrap.js"

  where
    script fileName =
      H.script_ [ H.src_ fileName ] ("" :: String)

    caption =
      H.div_ [ H.class_ "jumbotron" ] $
        H.h1_ "Rechner..."

    inputForm =
      H.form_ [ H.method_ "post"
              , H.class_ "form-inline"
              ] $ do
        H.div_ [ H.class_ "form-group" ] $
          H.input_ [ H.type_ "text"
                   , H.class_ "form-control"
                   , case result of
                       Just (formel,_) -> H.value_ $ T.pack formel
                       Nothing         -> H.placeholder_ "Formel..."
                   , H.autofocus_
                   , H.name_ "formel"
                   ]
        showResult
        H.button_ [ H.type_ "submit"
                  , H.class_ "btn btn-primary"
                  ] "calc"

    showResult =
      case result of
        Nothing  -> return ()
        Just (_, erg) -> H.div_ [ H.class_ "form-group" ] $
          H.input_ [ H.type_ "text"
                   , H.readonly_ ""
                   , H.class_ "form-control-plaintext"
                   , H.value_ $ T.pack $ " = " ++ show erg
                   ]
