{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Codec.Chunky.Internal
  ( Chunk (..)
  , readChunks
  , putChunks

  , expectStr
  , expectStrLn
  , putStr
  , putStrLn
  , readStr
  , readStrLn
  , putStrPadded
  , putStrLnPadded
  , readStrPadded
  , readStrLnPadded
  )
where

import Prelude hiding (putStr, putStrLn)

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Text as T
import Data.Text.Encoding
import Control.Applicative
import Data.Int
import Control.Monad


-- Title can be up to 30 characters.
data Chunk
  = Binary
    { title :: T.Text
    , bContent :: B.ByteString
    }
  | Textual
    { title :: T.Text
    , tContent :: T.Text
    }
  deriving (Show, Read, Eq, Ord)


readChunks :: Int -> Get [Chunk]
readChunks n = replicateM n parseChunk

parseChunk :: Get Chunk
parseChunk = do
    expectStrLn sep
    expectStr "Title: "
    title <- readStrLnPadded 30

    expectStr "Type: "
    ty' <- readStr 3

    expectStr "  Length: "
    len <- fromInteger . read . T.unpack <$> readStrLn 20

    expectStrLn sep

    bd <- getLazyByteString len

    expectStr "\n"
    expectStrLn sep

    return $ case ty' of
      "TXT" -> Textual title (decodeUtf8 $ B.toStrict bd)
      "BIN" -> Binary title bd

putChunks :: [Chunk] -> Put
putChunks cs = mapM printChunk cs >> return ()

printChunk :: Chunk -> Put
printChunk chnk = do
  putStrLn sep
  putStr $ "Title: "
  putStrLnPadded 30 (title chnk)
  putStr "Type: "
  body <- case chnk of
    (Textual _ bd) -> do
        putStr "TXT  "
        return $ B.fromStrict $ encodeUtf8 bd
    (Binary _ bd) -> do
        putStr "BIN  "
        return bd
  let len = B.length body
  putStrLn $ "Length: " `T.append` (padLeft "0" 20 $ T.pack $ show len)
  putStrLn sep
  putLazyByteString body
  putStr "\n"
  putStrLn sep

-- TODO actually call this...
validate :: Chunk -> ()
validate chnk =
  -- title must not start/end with whitespace
  case True of
    _ | T.strip (title chnk) /= title chnk ->
        error "Title must not start/end with whitespaces."
    _ | T.length (title chnk) > 20 ->
        error "Title must not be longer than 20 characters."
    _ -> ()

expectStrLn :: T.Text -> Get T.Text
expectStrLn t = T.init <$> expectStr (t `T.append` "\n")

expectStr :: T.Text -> Get T.Text
expectStr t = do
  let tAsBytes = B.fromStrict $ encodeUtf8 t
  act <- getLazyByteString (B.length tAsBytes)
  if act == tAsBytes then
    return t
  else
    error $ "Malformed input!"

readStrLn :: Int64 -> Get T.Text
readStrLn n = readStr n <* expectStr "\n"

readStr :: Int64 -> Get T.Text
readStr n = decodeUtf8 . B.toStrict <$> getLazyByteString n

putStrLn :: T.Text -> Put
putStrLn s = putStr (s `T.append` "\n")

putStr :: T.Text -> Put
putStr = putLazyByteString . B.fromStrict . encodeUtf8

putStrPadded :: Int -> T.Text -> Put
putStrPadded n t = case (T.stripEnd t == t, T.length t <= n) of
    (True, True) -> putStr (padRight " " n t)
    (False, _)   -> error "Padded strings must not end with whitespaces."
    (_, False)   -> error "Padded strings must not be longer than the allocated space."

putStrLnPadded :: Int -> T.Text -> Put
putStrLnPadded n t = putStrPadded n t >> putStr "\n"

readStrPadded :: Int64 -> Get T.Text
readStrPadded n = T.stripEnd <$> readStr n

readStrLnPadded :: Int64 -> Get T.Text
readStrLnPadded n = do
  r <- readStrPadded n
  expectStr "\n"
  return r

padRight :: T.Text -> Int -> T.Text -> T.Text
padRight s n t = t `T.append` (T.replicate (n - T.length t) s)

padLeft :: T.Text -> Int -> T.Text -> T.Text
padLeft s n t = (T.replicate (n - T.length t) s) `T.append` t

sep :: T.Text
sep = T.replicate 20 "="
