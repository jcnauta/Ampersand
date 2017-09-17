{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.PHP 
         ( evaluateExpSQL
         , signalTableSpec
         , sessionTableSpec
         , plug2TableSpec
         , quotedTableName,attributeNames
         , createTempDatabase
         , tempDbName
         , tableSpec2Queries
         , SqlQuery
         , sqlQuery2Text
         , additionalDatabaseSettings
         ) where

import Prelude hiding (exp,putStrLn,readFile,writeFile)
import Control.Exception
import Data.Monoid
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Process
import System.IO hiding (hPutStr,hGetContents,putStrLn,readFile,writeFile)
import System.Directory
import System.FilePath
import Ampersand.FSpec.SQL
import Ampersand.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug(suitableAsKey)
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Misc
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowAStruct

data TableSpec
  = TableSpec { tsCmnt :: [String]  -- Without leading "// "
              , tsName :: ADLText  --without quotes
              , tsflds :: [AttributeSpec]
              , tsKey  ::  SQLText
              }
data AttributeSpec
  = AttributeSpec { fsname :: SQLText
                  , fstype :: TType
                  , fsIsPrimKey :: Bool
                  , fsDbNull :: Bool
                  }


quotedTableName :: TableSpec -> SQLText
quotedTableName = safeObjectName . safeSQL . tsName
quotedAttName :: SqlAttribute -> SQLText
quotedAttName = safeObjectName . toSQL . Text.pack . attName
safeObjectName :: SQLText -> SQLText
safeObjectName = doubleQuote
safeLiteral :: SQLText -> SQLText
safeLiteral = singleQuote
attributeNames :: TableSpec -> [SQLText]
attributeNames = map fsname . tsflds

createTablePHP :: TableSpec -> [PHPText]
createTablePHP tSpec =
  map (toPHP . (("// "<>) . Text.pack)) (tsCmnt tSpec) <>
  [-- Drop table if it already exists
    toPHP "if($columns = mysqli_query($DB_link, "<>safePHP (toSQL "SHOW COLUMNS FROM "<>quotedTableName tSpec)<>")){"
  , toPHP "    mysqli_query($DB_link, "<>safePHP (toSQL "DROP TABLE "<>quotedTableName tSpec)<>");"
  , toPHP "}"
  ] <>
  [ toPHP "$sql="<>(safePHP . unlinesT $ createTableSql True tSpec)<>";"
  , toPHP "mysqli_query($DB_link,$sql);" 
  , toPHP "if($err=mysqli_error($DB_link)) {"
  , toPHP "  $error=true; echo $err.'<br />';"
  , toPHP "}"
  , toPHP ""
  ]

commentBlockSQL :: [String] -> [SQLText]
commentBlockSQL [] = []
commentBlockSQL xs = map (toSQL . Text.pack) .
   map (\cmmnt -> "/* "<>cmmnt<> " */") $ [hbar] <> map addSpaces xs <> [hbar]
  where 
    hbar = replicate (maximum . map length $ xs) '-'
    addSpaces str = str <> replicate (length hbar - length str) ' '
    
createTableSql :: Bool -> TableSpec -> [SQLText]
createTableSql withComment tSpec = 
      ( if withComment 
        then commentBlockSQL . tsCmnt $ tSpec
        else mempty
      ) <>
      [ toSQL "CREATE TABLE "<>(quotedTableName $ tSpec)] <>
      [ indnt <> toSQL prefx <> " " <> addColumn att 
      | (prefx, att) <- zip ("(" : repeat ",") (tsflds tSpec)] <>
      ( if nullT (tsKey tSpec) 
        then []
        else [ indnt <> toSQL ", " <> (tsKey tSpec) ]
      ) <>
      [ indnt <> toSQL ", " <> safeObjectName "ts_insertupdate"<>toSQL " TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]<>
      [ indnt <> toSQL ") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN" ]<>
      [ indnt <> toSQL ", ROW_FORMAT = DYNAMIC"]<>
      [ toSQL "" ]
  where
    indnt = toSQL (Text.replicate 5 " ") 
    addColumn :: AttributeSpec -> SQLText
    addColumn att 
       =    (safeObjectName . fsname $ att) 
         <> (toSQL " " )
         <> (showSQL . fstype $ att) 
         <> (if fsIsPrimKey att then toSQL " UNIQUE" else mempty)
         <> (if fsDbNull att then toSQL " DEFAULT NULL" else toSQL " NOT NULL")

plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = 
                   ["Plug "<>name plug
                   ,""
                   ,"attributes:"
                   ]<> concat
                   [ [showA (attExpr x)
                     , "  "<>(show.properties.attExpr) x ]
                   | x <- plugAttributes plug
                   ]
     , tsName = toADL . Text.pack $ name plug
     , tsflds = map fld2AttributeSpec $ plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> if all (suitableAsKey . attType) (plugAttributes plug)
                                    then (   toSQL "PRIMARY KEY (" 
                                          <> intercalateT (toSQL ", ") (map quotedAttName (plugAttributes plug))
                                          <> toSQL ")"
                                         )
                                    else mempty
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         PrimaryKey _ -> toSQL "PRIMARY KEY (" <> quotedAttName primFld <> ")"
                         ForeignKey c -> fatal ("ForeignKey "<>name c<>"not expected here!")
                         PlainAttr    -> ""
     }
fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = toSQL . Text.pack $ name att
                  , fstype = attType att
                  , fsIsPrimKey = isPrimaryKey att
                  , fsDbNull = attDBNull att 
                  }


signalTableSpec :: TableSpec
signalTableSpec =
    TableSpec { tsCmnt = ["Signal table"]
              , tsName = "__all_signals__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "conjId"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "src"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "tgt"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }        
                         ]
              , tsKey  = ""
              }

sessionTableSpec :: TableSpec
sessionTableSpec = 
    TableSpec { tsCmnt = ["Session timeout table"]
              , tsName = "__SessionTimeout__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "SESSION"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = True
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "lastAccess"
                             , fstype      = Integer --HJO: Why not DateTime???
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         ]
              , tsKey  = "PRIMARY KEY ("<>safeObjectName "SESSION"<>")"
              }


-- evaluate normalized exp in SQL
evaluateExpSQL :: FSpec -> SQLText -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec dbNm exp =
  -- verboseLn (getOpts fSpec) ("evaluateExpSQL fSpec "++showA exp)
  -- verboseLn (getOpts fSpec) (intercalate "\n" . showPrf showA . cfProof (getOpts fSpec)) exp
  -- verboseLn (getOpts fSpec) "End of proof"
  performQuery fSpec dbNm (violationsQuery)
 where violationsExpr = conjNF (getOpts fSpec) exp
       violationsQuery = prettySQLQuery 26 fSpec violationsExpr

performQuery :: FSpec -> SQLText -> SQLText -> IO [(String,String)]
performQuery fSpec dbNm query =
 do { queryResult <- executePHPText php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} ("\n******Problematic query:\n"<>showQuery<>"\n******")
              fatal ("PHP/SQL problem: "<>queryResult)
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal (  "Parse error on php result: \n"
                                    <>(unlines . indent 5 . lines $ queryResult)
                                    <>"\nOriginal query:\n"
                                    <>showQuery
                                   )
    } 
   where
    showQuery = unlines . showNumbered . lines . Text.unpack . toHaskellText $ query
    showNumbered :: [String] -> [String]
    showNumbered xs = map makeLine (zip [1::Int  ..] xs)
      where makeLine (nr, str) = "/*"<>show nr<>"*/"<>str
    opts = getOpts fSpec
    php :: [PHPText]
    php =
      connectToMySqlServerPHP opts (Just dbNm) <>
      [ toPHP "$sql="<>doubleQuote (safePHP query)<>";"
      , toPHP "$result=mysqli_query($DB_link,$sql);"
      , toPHP "if(!$result)"
      , toPHP "  die('Error : Connect to server failed'.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
      , toPHP "$rows=Array();"
      , toPHP "  while ($row = mysqli_fetch_array($result)) {"
      , toPHP "    $rows[]=$row;"
      , toPHP "    unset($row);"
      , toPHP "  }"
      , toPHP "echo '[';"
      , toPHP "for ($i = 0; $i < count($rows); $i++) {"
      , toPHP "  if ($i==0) echo ''; else echo ',';"
      , toPHP "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
      , toPHP "}"
      , toPHP "echo ']';"
      ]

-- call the command-line php with phpStr as input
executePHPText :: [PHPText] -> IO String
executePHPText phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " <> err)
                                 return ".")
    ; (tempPhpFile, temph) <- openTempFile tempdir "tmpPhpQueryOfAmpersand.php"
    ; Text.hPutStr temph (showPHP phpStr)
    ; hClose temph
    ; results <- executePHPFile tempPhpFile
  --  ; removeFile tempPhpFile
    ; return (normalizeNewLines results)
    }
  where
    showPHP :: [PHPText] -> Text.Text
    showPHP phpLines = Text.unlines $ ["<?php"]<>map toHaskellText phpLines<>["?>"]



normalizeNewLines :: String -> String
normalizeNewLines = f . intercalate "\n" . lines
  where 
    f [] = []
    f ('\r':'\n':rest) = '\n':f rest
    f (c:cs) = c: f cs 

executePHPFile :: String -> IO String
executePHPFile phpPath =
 do { let cp = (shell command) 
                   { cwd = Just (takeDirectory phpPath)
                   }
          inputFile = phpPath
          outputFile = inputFile++"Result"
          command = "php "++show inputFile++" > "++show outputFile
    ; _ <- readCreateProcess cp ""
    ; result <- readFile outputFile
    ; removeFile outputFile
    ; return result
    }

tempDbName :: Options -> SQLText
tempDbName opts = fromHaskellText . Text.pack $ "TempDB_"<>dbName opts

connectToMySqlServerPHP :: Options -> Maybe SQLText-> [PHPText]
connectToMySqlServerPHP opts mDbName =
    [ toPHP "// Try to connect to the MySQL server"
    , toPHP "global $DB_host,$DB_user,$DB_pass;"
    , toPHP "$DB_host="<>(toPHP . Text.pack . sqlHost  $ opts)<>";"
    , toPHP "$DB_user="<>(toPHP . Text.pack . sqlLogin $ opts)<>";"
    , toPHP "$DB_pass="<>(toPHP . Text.pack . sqlPwd   $ opts)<>";"
    , toPHP ""
    ]<>
    (case mDbName of
       Nothing   ->
         [ toPHP "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
         , toPHP "// Check connection"
         , toPHP "if (mysqli_connect_errno()) {"
         , toPHP "  die('Failed to connect to MySQL: ' . mysqli_connect_error());"
         , toPHP "}"
         , toPHP ""
         ]
       Just dbNm ->
         [toPHP "$DB_name="<>doubleQuote (safePHP dbNm)<>toPHP ";"]<>
         connectToTheDatabasePHP
    )

connectToTheDatabasePHP :: [PHPText]
connectToTheDatabasePHP =
    [ toPHP "// Connect to the database"
    , toPHP "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
    , toPHP "// Check connection"
    , toPHP "if (mysqli_connect_errno()) {"
    , toPHP "  die('Error : Failed to connect to the database: ' . mysqli_connect_error());"
    , toPHP "  }"
    , toPHP ""
    ]<>
    [ toPHP "$sql="<>safePHPString(toSQL "SET SESSION sql_mode = "<>safeObjectName "ANSI,TRADITIONAL") <>";"
                                                            -- ANSI because of the syntax of the generated SQL
                                                            -- TRADITIONAL because of some more safety
    , toPHP "if (!mysqli_query($DB_link,$sql)) {"
    , toPHP "  die('Error setting sql_mode: ' . mysqli_error($DB_link));"
    , toPHP "  }"
    , toPHP ""
    ]
safePHPString :: SQLText -> PHPText
safePHPString = doubleQuote . safePHP
createTempDatabase :: FSpec -> IO ()
createTempDatabase fSpec =
 do { --dump ">>>INPUT>>>" (Text.lines $ showPHP phpStr) 
    ; result <- executePHPText phpStr
    ; --dump "<<<OUTPUT<<<" (Text.lines . Text.pack $ result)
    ; verboseLn (getOpts fSpec) 
         (if null result 
          then "Temp database created succesfully."
          else "Temp database creation failed! :"<>result  )
    }
 where 
--  dump :: String -> [Text.Text] -> IO ()
--  dump prefix txt = mapM_ (verboseLn $ getOpts fSpec) noot
--    where
--      noot :: [String]
--      noot = map aap (zip [1..99] txt)
--      aap :: (Int, Text.Text) -> String
--      aap (i,x) = prefix <> " "<>(show i)<>" "<>Text.unpack x


  phpStr :: [PHPText]
  phpStr = 
    connectToMySqlServerPHP (getOpts fSpec) Nothing <>
    [ toPHP "/*** Set global varables to ensure the correct working of MySQL with Ampersand ***/"
    , toPHP ""
    , toPHP "    /* file_per_table is required for long columns */"
    , toPHP "    $sql="<>safePHPString(toSQL "SET GLOBAL innodb_file_per_table = true")<>";"
    , toPHP "    $result=mysqli_query($DB_link, $sql);"
    , toPHP "       if(!$result)"
    , toPHP "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , toPHP "" 
    , toPHP "    /* file_format = Barracuda is required for long columns */"
    , toPHP "    $sql="<>safePHPString(toSQL "SET GLOBAL innodb_file_format = "<>safeLiteral "Barracuda")<>";"
    , toPHP "    $result=mysqli_query($DB_link, $sql);"
    , toPHP "       if(!$result)"
    , toPHP "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , toPHP ""
    , toPHP "    /* large_prefix gives max single-column indices of 3072 bytes = win! */"
    , toPHP "    $sql="<>safePHPString(toSQL "SET GLOBAL innodb_large_prefix = true")<>";"
    , toPHP "    $result=mysqli_query($DB_link, $sql);"
    , toPHP "       if(!$result)"
    , toPHP "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , toPHP ""
    ]<> 
    [ toPHP "$DB_name="<>safePHPString (tempDbName (getOpts fSpec))<>";"
    , toPHP "// Drop the database if it exists"
    , toPHP "$sql="<>safePHPString(toSQL "DROP DATABASE $DB_name"<>";")
    , toPHP "mysqli_query($DB_link,$sql);"
    , toPHP "// Don't bother about the error if the database didn't exist..."
    , toPHP ""
    , toPHP "// Create the database"
    , toPHP "$sql="<>safePHPString(toSQL "CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin")<>";"
    , toPHP "if (!mysqli_query($DB_link,$sql)) {"
    , toPHP "  die('Error creating the database: ' . mysqli_error($DB_link));"
    , toPHP "  }"
    , toPHP ""
    ] <>
    connectToTheDatabasePHP <>       
    [ toPHP "/*** Create new SQL tables ***/"
    , toPHP ""
    ] <>
    createTablePHP signalTableSpec <>
    createTablePHP sessionTableSpec <>
    [ toPHP ""
    , toPHP "//// Number of plugs: " <> (fromHaskellText . Text.pack . show . length . plugInfos $ fSpec)
    ]
    -- Create all plugs
    <> concat [createTablePHP . plug2TableSpec $ p | InternalPlug p <- plugInfos fSpec]
    -- Populate all plugs
    <> concat [populatePlugPHP p | InternalPlug p <- plugInfos fSpec]
  
    where
      populatePlugPHP :: PlugSQL -> [PHPText]
      populatePlugPHP plug =
        case tableContents fSpec plug of
          [] -> []
          tblRecords 
             -> [ toPHP "$sqlQuery="<>
                         safePHPString ( toSQL "INSERT INTO "<>(safeObjectName . toSQL . Text.pack . name $ plug)
                                       <>toSQL " ("<>intercalateT (toSQL ",") (map quotedAttName (plugAttributes plug))<>toSQL ")"
                                       <>indnt 14<>toSQL "VALUES " <> intercalateT (indnt 19<>toSQL ", ") [ toSQL "(" <>valuechain md<>toSQL ")" | md<-tblRecords]
                                       <>indnt 10
                                       )<>";"
                , toPHP "mysqli_query($DB_link,$sqlQuery);"
                , toPHP "if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"
                ]
       where
        indnt :: Int -> SQLText
        indnt i =
         toSQL . Text.pack $ '\n':replicate i ' '
        valuechain :: [Maybe AAtomValue] -> SQLText
        valuechain record = 
           intercalateT ", " 
             [case att of 
                 Nothing  -> toSQL "NULL" 
                 Just val -> toSQLTxt val 
             | att<-record
             ]


-- *** MySQL stuff below:

data SqlQuery = SqlQuery [SQLText]

tableSpec2Queries :: Bool -> TableSpec -> [SqlQuery]
tableSpec2Queries withComment tSpec = 
 (SqlQuery $ createTableSql withComment tSpec
 ):
 [SqlQuery [    toSQL "CREATE INDEX "<> safeObjectName (quotedTableName tSpec<>"_"<>fsname att)
             <> toSQL " ON "<>quotedTableName tSpec
             <> toSQL " ("<>doubleQuote (fsname att)<>toSQL ")"
           ]
 | att <- tsflds tSpec
 , not (fsIsPrimKey att)
 , suitableAsKey (fstype  att)
 ]

additionalDatabaseSettings :: [SqlQuery]
additionalDatabaseSettings = [ SqlQuery ["SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]]

sqlQuery2Text :: Bool -> SqlQuery -> SQLText
sqlQuery2Text withComment (SqlQuery ts)
   = if withComment 
     then unlinesT ts <>";"
     else unwordsT . wordsT . unlinesT $ ts

