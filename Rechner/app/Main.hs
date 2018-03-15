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
main =
  putStrLn "Hello Haskell"
