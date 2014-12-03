-- | Example output of this library looks like this, opened in a simple text editor (gedit):
--
-- @
-- ====================
-- Title: Test Textual        
-- Type: TXT  Length: 00000000000000000022
-- ====================
-- Blabsdfsdf
--
-- afsdfsadfs
-- ====================
-- ====================
-- Title: Test Binary         
-- Type: BIN  Length: 00000000000000000014
-- ====================
-- bladsfdsg
--
-- dfg
-- ====================
-- ====================
-- Title: Test Binary2        
-- Type: BIN  Length: 00000000000000000007
-- ====================
-- \FF\FF\FF﻿\00
-- ====================
-- @
--
-- You should make sure that the file is written in binary mode, e.g. use
-- the readFile and writeFile methods from Data.ByteString.Lazy.
--
module Codec.Chunky
  ( -- * Support for writing/reading chunks
    Chunk (..)
  , readChunks
  , putChunks

  -- * Some useful helper functions when parsing/writing simple binary files.
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

import Prelude ()

import Codec.Chunky.Internal

