{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Temp where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)

import           AST                    (AExp, BExp, Com, factorial)

test :: IO ()
test = putStrLn "Bleh"
