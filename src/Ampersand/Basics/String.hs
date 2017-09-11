{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
  -- | This module contains some common String funcions
module Ampersand.Basics.String 
        ( unCap,upCap
        , escapeNonAlphaNum
        , escapeIdentifier
        , optionalQuote
        , singleQuote,doubleQuote
        , SafeText(..)
        ) where

import Data.Char
import Data.Monoid
import Data.String(IsString)
import qualified Data.Text as Text

-- | Converts the first character of a string to lowercase, with the exception that there is a second character, which is uppercase.
-- uncap "AbcDe" == "abcDe"
-- uncap "ABcDE" == "ABcDE"
unCap :: String -> String
unCap [] = []
unCap [h] = [toLower h]
unCap (h:h':t) | isUpper h' = h:h':t
               | otherwise  = toLower h:h':t
-- | Converts the first character of a string to uppercase
upCap :: String -> String
upCap [] = []
upCap (h:t) = toUpper h:t

-- | escape anything except regular characters and digits to _<character code>
-- e.g. escapeNonAlphaNum "a_é" = "a_95_233"
escapeNonAlphaNum :: String -> String
escapeNonAlphaNum = concatMap escapeNonAlphaNumChar
 where escapeNonAlphaNumChar c
         | isAlphaNum c && isAscii c = [c]
         | otherwise                 = '_' : show (ord c)

-- Create an identifier that does not start with a digit and consists only of upper/lowercase ascii letters, underscores, and digits.
-- This function is injective.
escapeIdentifier :: String -> String  -- TODO: get rid of this function. It has multiple concerns. In it's usage, different problems are solve. Very confusing! (and thus probably wrong)
escapeIdentifier ""      = "_EMPTY_"
escapeIdentifier (c0:cs) = encode False c0 ++ concatMap (encode True) cs
  where encode allowNum c | isAsciiLower c || isAsciiUpper c || allowNum && isDigit c = [c]
                          | c == '_'  = "__" -- shorthand for '_' to improve readability
                          | otherwise = "_" ++ show (ord c) ++ "_"

optionalQuote :: String -> String
optionalQuote str
  | needsQuotes = show str
  | otherwise   = str 
 where
  needsQuotes =
   case words str of
    []  -> True
    [_] -> False
    _   -> True

singleQuote :: (IsString m, Monoid m) => m -> m
singleQuote str = "'"<>str<>"'"

doubleQuote :: (IsString m, Monoid m) => m -> m
doubleQuote s = "\"" <> s <> "\""

        
        
class (IsString a, Monoid a) => SafeText a where
-- https://www.ibm.com/support/knowledgecenter/en/SSBJG3_2.5.0/com.ibm.gen_busug.doc/c_fgl_sql_programming_080.htm says:
-- The ANSI SQL standards define doubles quotes as database object names delimiters, while single quotes are dedicated to string literals:
--   CREATE TABLE "my table" ( "column 1" CHAR(10) ) 
--   SELECT COUNT(*) FROM "my table" WHERE "column 1" = 'abc'
-- If you want to write a single quote character inside a string literal, you must write 2 single quotes:
--   ... WHERE comment = 'John''s house'        
  safeSQLObjectName :: a -> a
  safeSQLObjectName = doubleQuote . safeSQL
  safeSQLLiteral    :: a -> a
  safeSQLLiteral    = singleQuote . safeSQL
  safeSQL :: a -> a

-- | This function takes a string and puts quotes on them in an PHP specific way. 
--   it will also make sure that any character in the original string that
--   causes some conflict wrt PHP is properly escaped. This way, the result
--   is a proper string that can be used in any PHP statement.  
  safePHPString     :: a -> a
  safePHPString     = doubleQuote . safePHP
  safePHP :: a -> a


instance SafeText Text.Text where
  safeSQL str = 
    case Text.uncons str of
      Nothing -> Text.empty
      Just (c , cs) 
        | c == '\'' -> "''" <> safeSQL cs
        | c == '\"' -> "\"" <> safeSQL cs
        | otherwise -> c `Text.cons` safeSQL cs
  safePHP str = 
    case Text.uncons str of
      Nothing -> Text.empty
      Just (c , cs) 
        | c == '\"' -> "\\\"" <> safePHP cs
        | c == '\\' -> "\\" <> safePHP cs
        | otherwise -> c `Text.cons` safePHP cs
  
instance SafeText String where
  safeSQL str = 
    case str of
      [] -> []
      (c:cs) 
        | c == '\'' -> "''" <> safeSQL cs
        | c == '\"' -> "\"" <> safeSQL cs
        | otherwise -> c : safeSQL cs
  safePHP str = 
    case str of
      [] -> []
      (c:cs) 
        | c == '\"' -> "\\\"" <> safePHP cs
        | c == '\\' -> "\\" <> safePHP cs
        | otherwise -> c : safePHP cs