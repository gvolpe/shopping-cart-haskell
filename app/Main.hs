{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Http.Server                    ( runServer )
import           Resources
import qualified Services.Brands               as SB
import qualified Services.Items                as SI
import           Services

main :: IO ()
main = do
  res    <- mkResources
  brands <- SB.mkLiveBrands (psql res)
  items  <- SI.mkLiveItems (psql res)
  runServer (Services brands items)
