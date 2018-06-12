-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

-- | Just to give `tr` a more descriptive type
type CharSet = String
type CharMap = [(Char, Char)]

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
tr inset Nothing xs       = deleteFrom inset xs
tr inset (Just outset) xs = translate (equalizeAndZip inset outset) xs
tr _inset _outset xs      = xs

deleteFrom :: CharSet -> String -> String
deleteFrom inset xs = filter (\a -> not $ elem a inset) xs

translate :: CharMap -> String -> String
translate cmap input = ""

equalizeAndZip :: [a] -> [b] -> [(a, b)]
equalizeAndZip lst1 lst2 
  | len1 == len2 = zip lst1 lst2
  | null lst2     = undefined
  | len1 > len2  = zip lst1 (extendBy lst2 len1)
  | otherwise    = zip lst1 (take len1 lst2)
  where len1 = length lst1
        len2 = length lst2

extendBy :: [a] -> Int -> [a]
extendBy lst n
  | len > n   = error "The list is longer than the desired length."
  | otherwise = lst ++ replicate (n - len) (last lst)
  where len = length lst
