{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
  -- | This module contains some common String funcions
module Ampersand.Basics.String 
        ( unCap,upCap
        , escapeNonAlphaNum
        , escapeIdentifier
        , quoteWhenMultipleWords
        , ADLText(ADLText),SQLText,PHPText
        , SafeConvert(..)
        , toPHP, toSQL, toADL
        , singleQuote
        , doubleQuote
        , sqlLiteral, sqlObjectName
        ) where

import Ampersand.Basics.Version (fatal)
import Data.Char
import Data.Monoid
import Data.String(IsString(..))
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

quoteWhenMultipleWords :: String -> String
quoteWhenMultipleWords str
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

data PHPText = PHPText Text.Text deriving (Eq,Ord)
data SQLText = SQLText Text.Text deriving (Eq,Ord)
data ADLText = ADLText Text.Text deriving (Eq,Ord)

instance Monoid PHPText where
  mempty = PHPText Text.empty
  mappend (PHPText a) (PHPText b) = PHPText (mappend a b)

instance Monoid SQLText where
  mempty = SQLText Text.empty
  mappend (SQLText a) (SQLText b) = SQLText (mappend a b)

instance Monoid ADLText where
  mempty = ADLText Text.empty
  mappend (ADLText a) (ADLText b) = ADLText (mappend a b)

instance IsString PHPText where
  fromString = PHPText . Text.pack 

instance IsString SQLText where
  fromString = SQLText . Text.pack 

instance IsString ADLText where
  fromString = ADLText . Text.pack 

class (IsString a, Monoid a) => SafeConvert a where
  fromHaskellText :: Text.Text -> a
  toHaskellText :: a -> Text.Text
  safeSQL :: a -> SQLText
  safePHP :: a -> PHPText
  safeADL :: a -> ADLText
  unwordsT :: [a] -> a 
  unwordsT = fromHaskellText . Text.unwords . map toHaskellText
  wordsT :: a -> [a]
  wordsT = map fromHaskellText . Text.words . toHaskellText
  intercalateT :: a -> [a] -> a
  intercalateT txt ts = 
     fromHaskellText $ Text.intercalate (toHaskellText txt) (map toHaskellText ts) 
  linesT :: a -> [a]
  linesT = map fromHaskellText . Text.lines . toHaskellText
  unlinesT :: [a] -> a
  unlinesT = fromHaskellText . Text.unlines . map toHaskellText
  nullT :: a -> Bool
  nullT = Text.null . toHaskellText
  replicateT :: Int -> a -> a
  replicateT i = fromHaskellText . Text.replicate i . toHaskellText
  lengthT :: a -> Int
  lengthT = Text.length . toHaskellText
instance SafeConvert SQLText where
  fromHaskellText = SQLText . convertTo SQL
  toHaskellText (SQLText txt) = convertFrom SQL txt
  safeSQL = fatal "Trying to convert SQLText to SQLText. This could be done with `id`, but a fatal has temporarily been used just to keep track of these things..." 
  safePHP = fromHaskellText . toHaskellText
  safeADL = fromHaskellText . toHaskellText 
instance SafeConvert PHPText where
  fromHaskellText = PHPText . convertTo PHP
  toHaskellText (PHPText txt) = convertFrom PHP txt
  safeSQL = fromHaskellText . toHaskellText
  safePHP = fatal "Trying to convert PHPText to PHPText. This could be done with `id`, but a fatal has temporarily been used just to keep track of these things..." 
  safeADL = fromHaskellText . toHaskellText
instance SafeConvert ADLText where
  fromHaskellText = ADLText . convertTo ADL
  toHaskellText (ADLText txt) = convertFrom ADL txt
  safeSQL = fromHaskellText . toHaskellText
  safePHP = fromHaskellText . toHaskellText
  safeADL = fatal "Trying to convert ADLText to ADLText. This could be done with `id`, but a fatal has temporarily been used just to keep track of these things..." 
data TextType = PHP | SQL | ADL

toPHP :: Text.Text -> PHPText
toPHP = fromHaskellText
toSQL :: Text.Text -> SQLText
toSQL = fromHaskellText
toADL :: Text.Text -> ADLText
toADL = fromHaskellText

convertTo :: TextType -> Text.Text -> Text.Text
convertTo SQL txt = 
  case Text.uncons txt of
    Nothing -> mempty
    Just (c , cs) 
      | c == '\'' -> "''" <>       convertTo SQL cs
      | c == '\"' -> "\"" <>       convertTo SQL cs
      | c == '\\' -> "\\" <>       convertTo SQL cs
      | otherwise -> c `Text.cons` convertTo SQL cs
convertTo PHP txt =
  case Text.uncons txt of
    Nothing -> Text.empty
    Just (c , cs) 
      | c == '\"' -> "\"" <>       convertTo PHP cs
      | c == '\\' -> "\\" <>       convertTo PHP cs
      | otherwise -> c `Text.cons` convertTo PHP cs
convertTo ADL txt =
  case Text.uncons txt of
    Nothing -> Text.empty
    Just (c , cs)
      | c == '\'' -> "''" <>       convertTo ADL cs
      | otherwise -> c `Text.cons` convertTo ADL cs

convertFrom :: TextType -> Text.Text -> Text.Text
convertFrom SQL txt = 
-- https://www.ibm.com/support/knowledgecenter/en/SSBJG3_2.5.0/com.ibm.gen_busug.doc/c_fgl_sql_programming_080.htm says:
-- The ANSI SQL standards define doubles quotes as database object names delimiters, while single quotes are dedicated to string literals:
--   CREATE TABLE "my table" ( "column 1" CHAR(10) ) 
--   SELECT COUNT(*) FROM "my table" WHERE "column 1" = 'abc'
-- If you want to write a single quote character inside a string literal, you must write 2 single quotes:
--   ... WHERE comment = 'John''s house'        
  case Text.uncons txt of
    Nothing -> mempty
    Just (c , cs)
      | c == '\'' -> case Text.uncons cs of
                       Nothing -> mempty
                       Just (c' , cs')
                         | c' == '\'' -> "'" <> convertFrom SQL cs'
                         | otherwise  ->        convertFrom SQL cs'
      | c == '\\' -> case Text.uncons cs of
                       Nothing -> fatal "Last character of SQLText is '\\', which should not happen!"
                       Just (c' , cs')
                         | c' == '\"' -> "\\\"" <> convertFrom SQL cs'
                         | c' == '\\' -> "\\\\" <> convertFrom SQL cs'
                         | otherwise  -> fatal $ "A character of SQL Text is '"++[c]++"', followed by a character '"++[c']++"' chr("++show(ord c')++"), which should not happen!"
      | otherwise -> c `Text.cons` convertFrom SQL cs
convertFrom PHP txt =
  case Text.uncons txt of
    Nothing -> mempty
    Just (c , cs)
      | c == '\\' -> case Text.uncons cs of
                       Nothing -> fatal "Last character of PHPText is '\\', which should not happen!"
                       Just (c' , cs')
                         | c' == '\"' -> "\\\"" <> convertFrom PHP cs'
                         | c' == '\\' -> "\\\\" <> convertFrom PHP cs'
                         | otherwise  -> fatal $ "A character of PHP Text is '"++[c]++"', followed by a character '"++[c']++"' chr("++show(ord c')++"), which should not happen!"
      | otherwise -> c `Text.cons` convertFrom PHP cs
convertFrom ADL txt =
  case Text.uncons txt of
    Nothing -> mempty
    Just (c , cs)
      | c == '\'' -> case Text.uncons cs of
                       Nothing -> mempty
                       Just (c' , cs')
                         | c' == '\'' -> "'" <> convertFrom ADL cs'
                         | otherwise  -> fatal $ "Single quote in an ADL Text should not be possible.\n"<>show txt
      | otherwise -> c `Text.cons` convertFrom ADL cs


{- class (IsString a, Monoid a) => SafeText a where
  safeSQLObjectName :: a -> SQLText
  safeSQLObjectName = doubleQuote . safeSQL
  safeSQLLiteral    :: a -> SQLText
  safeSQLLiteral    = singleQuote . safeSQL
  safeSQL :: a -> a

-- | This function takes a string and puts quotes on them in an PHP specific way. 
--   it will also make sure that any character in the original string that
--   causes some conflict wrt PHP is properly escaped. This way, the result
--   is a proper string that can be used in any PHP statement.  
  safePHPString     :: a -> a
  safePHPString     = doubleQuote . safePHP
  safePHP :: a -> a

  safeADLString     :: a -> a
  safeADLString     = doubleQuote . safeADL
  safeADL :: a -> a

instance SafeText Text.Text where
  safeSQL str = 
    case Text.uncons str of
      Nothing -> Text.empty
      Just (c , cs) 
        | c == '\'' -> "''" <> safeSQL cs
        | c == '\"' -> "\"" <> safeSQL cs
        | c == '\\' -> "\\" <> safeSQL cs
        | otherwise -> c `Text.cons` safeSQL cs
  safePHP str = 
    case Text.uncons str of
      Nothing -> Text.empty
      Just (c , cs) 
        | c == '\"' -> "\\\"" <> safePHP cs
        | c == '\\' -> "\\\\" <> safePHP cs
        | otherwise -> c `Text.cons` safePHP cs
  safeADL str =
    case Text.uncons str of
      Nothing -> Text.empty
      Just (c , cs)
        | c == '\'' -> "\'" <> safeADL cs
        | c == '\"' -> "\"" <> safeADL cs
        | c == '\\' -> "\\" <> safeADL cs
        | otherwise -> c `Text.cons` safeADL cs

instance SafeText String where
  safeSQL str = 
    case str of
      [] -> []
      (c:cs) 
        | c == '\'' -> "''" <> safeSQL cs
        | c == '\"' -> "\"" <> safeSQL cs
        | c == '\\' -> "\\" <> safeSQL cs
        | otherwise -> c : safeSQL cs
  safePHP str = 
    case str of
      [] -> []
      (c:cs) 
        | c == '\"' -> "\\\"" <> safePHP cs
        | c == '\\' -> "\\" <> safePHP cs
        | otherwise -> c : safePHP cs
  safeADL str =
    case str of
      [] -> []
      (c:cs) 
        | c == '\'' -> "\'" <> safeADL cs
        | c == '\"' -> "\"" <> safeADL cs
        | c == '\\' -> "\\" <> safeADL cs
        | otherwise -> c : safeADL cs

 -}

sqlLiteral :: SQLText -> SQLText
sqlLiteral (SQLText txt) = SQLText (singleQuote txt)

sqlObjectName :: SQLText -> SQLText
sqlObjectName (SQLText txt) = SQLText (doubleQuote txt)