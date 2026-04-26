{-|
Module      : Test.Tasty.HieCache.Internal
Description : Internal source-navigation and path helpers
Stability   : internal
Portability : portable

Internal utilities re-exported for testing.  Not part of the public API;
may change without notice between versions.
-}
module Test.Tasty.HieCache.Internal
  ( -- * Source navigation
    findExprEnd
  , countIndent
  , skipLine
  , findLineStart
  , lineStartOffset
  , findSubstring
    -- * Path helpers
  , TestPath
  , pathKey
  ) where

import qualified Data.ByteString as BS
import           Data.List       (intercalate)

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

-- | Byte offset of the start of the line containing @offset@.
findLineStart :: BS.ByteString -> Int -> Int
findLineStart src offset = go (offset - 1)
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i < 0                = 0
      | BS.index src i == nl = i + 1
      | otherwise            = go (i - 1)

-- | Byte offset just past the end of an indentation-delimited expression
-- starting at @lineStart@ with base indent @baseIndent@.  The expression
-- extends through every following line whose indent is strictly greater
-- than @baseIndent@.
findExprEnd :: BS.ByteString -> Int -> Int -> Int
findExprEnd src lineStart baseIndent = go (skipLine src lineStart)
  where
    go pos
      | pos >= BS.length src             = BS.length src
      | countIndent src pos > baseIndent = go (skipLine src pos)
      | otherwise                        = pos

-- | Count leading whitespace at byte offset @pos@.  Spaces count as 1 and
-- tabs as 8.  Returns @0@ if @pos@ is on a newline.
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

-- | Byte offset of the character after the next newline at or after @pos@.
-- Returns the byte length of @src@ if no newline is found.
skipLine :: BS.ByteString -> Int -> Int
skipLine src = go
  where
    nl = fromIntegral (fromEnum '\n')
    go i
      | i >= BS.length src   = BS.length src
      | BS.index src i == nl = i + 1
      | otherwise            = go (i + 1)

-- | Byte offset of the first occurrence of @needle@ in @haystack@,
-- or 'Nothing' if @needle@ does not appear.
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

-- | A path identifying a single test within the test tree, as the list of
-- group names from the root down to (and including) the leaf test name.
type TestPath = [String]

-- | Render a 'TestPath' as a single human-readable key, joining segments
-- with @\" > \"@.  Used as the lookup key in the on-disk cache.
pathKey :: TestPath -> String
pathKey = intercalate " > "
