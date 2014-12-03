{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import qualified Codec.Chunky as C
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put

chunks :: [C.Chunk]
chunks = [ C.Textual "Test Textual" "Blabsdfsdf\n\nafsdfsadfs"
        , C.Binary "Test Binary" (B.fromStrict $ encodeUtf8 "bladsfdsg\n\ndfg")
        , C.Binary "Test Binary2" (B.pack [0xFF, 0xFF, 0xFF, 0xEF, 0xBB, 0xBF, 0x00])
        ]

main :: IO ()
main = do
  B.writeFile "test.chnk" $ runPut $ C.putChunks chunks
  rr <- runGet (C.readChunks (length chunks)) <$> B.readFile "test.chnk"
  if rr == chunks then
    putStrLn "Got back correct result!"
  else
    putStrLn "Wrong result !?"
