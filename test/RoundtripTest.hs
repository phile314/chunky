{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Codec.Chunky
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as B
import Test.HUnit
import System.Exit
import Data.Binary.Get
import Data.Binary.Put

chunks :: [Chunk]
chunks = [ Textual "Test Textual" "Blabsdfsdf\n\nafsdfsadfs"
        , Binary "Test Binary" (B.fromStrict $ encodeUtf8 "bladsfdsg\n\ndfg")
        , Binary "Test Binary2" (B.pack [0xFF, 0xFF, 0xFF, 0xEF, 0xBB, 0xBF, 0x00])
        ]

tests = TestList [TestLabel "Roundtrip Test" rndtripTest]

rndtripTest = TestCase (do
  let bs = runPut $ putChunks chunks
  let act = runGet (readChunks (length chunks)) bs
  
  assertEqual "" chunks act
  )

main :: IO ()
main = do
  cnts <- runTestTT $ tests
  if (errors cnts > 0 || failures cnts > 0) then
    exitWith (ExitFailure 1)
  else
    exitSuccess
