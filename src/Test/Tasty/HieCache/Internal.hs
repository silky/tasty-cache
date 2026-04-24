-- | Internal utilities re-exported for testing.  Not part of the public API;
-- may change without notice between versions.
module Test.Tasty.HieCache.Internal
  ( -- * Source navigation
    findExprEnd
  , countIndent
  , skipLine
  , findLineStart
  , lineStartOffset
  , findSubstring
    -- * Path helpers
  , pathKey
  ) where

import qualified Data.ByteString       as BS
import           Data.List             (intercalate)

-- ---------------------------------------------------------------------------
-- Source navigation

-- | Byte offset of the start of line @n@ (1-indexed).
lineStartOffset :: BS.ByteString -> Int -> Int
lineStartOffset src n = go 0 1
  where
    nl = fromIntegral (fromEnum '\n')
    go pos line
      | line >= n            = pos
      | pos >= BS.length src = pos
      | BS.index src pos == nl = go (pos + 1) (line + 1)
      | otherwise              = go (pos + 1) line

findLineStart :: BS.ByteString -> Int -> Int
findLineStart src offset = go (offset - 1)
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i < 0                = 0
      | BS.index src i == nl = i + 1
      | otherwise            = go (i - 1)

findExprEnd :: BS.ByteString -> Int -> Int -> Int
findExprEnd src lineStart baseIndent = go (skipLine src lineStart)
  where
    go pos
      | pos >= BS.length src             = BS.length src
      | countIndent src pos > baseIndent = go (skipLine src pos)
      | otherwise                        = pos

countIndent :: BS.ByteString -> Int -> Int
countIndent src pos = go pos 0
  where
    sp = fromIntegral (fromEnum ' ')
    tb = fromIntegral (fromEnum '\t')
    nl = fromIntegral (fromEnum '\n')
    go i n
      | i >= BS.length src   = n
      | BS.index src i == sp = go (i + 1) (n + 1)
      | BS.index src i == tb = go (i + 1) (n + 8)
      | BS.index src i == nl = 0
      | otherwise            = n

skipLine :: BS.ByteString -> Int -> Int
skipLine src = go
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i >= BS.length src   = BS.length src
      | BS.index src i == nl = i + 1
      | otherwise            = go (i + 1)

findSubstring :: BS.ByteString -> BS.ByteString -> Maybe Int
findSubstring needle haystack
  | BS.null needle = Just 0
  | otherwise      = go 0
  where
    nlen = BS.length needle
    hlen = BS.length haystack
    go i
      | i + nlen > hlen                             = Nothing
      | BS.take nlen (BS.drop i haystack) == needle = Just i
      | otherwise                                   = go (i + 1)

-- ---------------------------------------------------------------------------
-- Path helpers

type TestPath = [String]

pathKey :: TestPath -> String
pathKey = intercalate " > "
