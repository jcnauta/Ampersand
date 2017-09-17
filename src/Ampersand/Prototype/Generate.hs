{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries, generateMetaPopQueries
  )
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree 
import Ampersand.FSpec
import Ampersand.Prototype.PHP
import Data.Monoid
import qualified Data.Text as Text
import Prelude hiding (writeFile,readFile,getContents,exp)


generateDBstructQueries :: FSpec -> Bool -> [SQLText]
generateDBstructQueries fSpec withComment = 
   map (sqlQuery2Text withComment) $ generateDBstructQueries' fSpec withComment

generateDBstructQueries' :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries' fSpec withComment 
  =    concatMap (tableSpec2Queries withComment)
                 ( sessionTableSpec
                 : signalTableSpec
                 : [plug2TableSpec p | InternalPlug p <- plugInfos fSpec]
                 )
    <> additionalDatabaseSettings 
generateInitialPopQueries :: FSpec -> [SQLText]
generateInitialPopQueries fSpec 
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops False fSpec
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [SQLText]
    fillSignalTable [] = mempty
    fillSignalTable conjSignals 
     = [unlinesT
            [ toSQL "INSERT INTO "<>(quotedTableName signalTableSpec)
            , toSQL "   ("<>intercalateT (toSQL ", ") (map sqlObjectName (attributeNames signalTableSpec))<>")"
            , toSQL "VALUES " <> intercalateT (toSQL " , ") 
                  [ "(" <>intercalateT (toSQL ", ") [(sqlLiteral . toSQL . Text.pack . rc_id $ conj)
                                                    ,(sqlLiteral . toSQLTxt . apLeft  $ p)
                                                    ,(sqlLiteral . toSQLTxt . apRight $ p)
                                                    ]<> ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
            ]
       ]
generateMetaPopQueries :: FSpec -> [SQLText]
generateMetaPopQueries = populateTablesWithPops True

populateTablesWithPops :: Bool -> FSpec -> [SQLText]
populateTablesWithPops ignoreDoubles fSpec =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [SQLText]
        populatePlug plug = map insertRecord . tableContents fSpec $ plug 
          -- = case tableContents fSpec plug of
          --    []  -> []
          --    tblRecords 
          --        -> [Text.unlines
          --              [ "INSERT "<> (if ignoreDoubles then "IGNORE " else "") <>"INTO "
          --                    <>Text.pack (show (name plug))
          --              , "   ("<>Text.intercalate ", " (map (Text.pack . show . attName) (plugAttributes plug))<>") "
          --              , "VALUES " <> Text.intercalate " , " 
          --                 [ "(" <>valuechain md<> ")" | md<-tblRecords]
          --              ]
          --           ]
         where
           insertRecord :: [Maybe AAtomValue] -> SQLText
           insertRecord md = 
                unlinesT
                  [ toSQL "INSERT "<> (if ignoreDoubles then toSQL "IGNORE " else mempty) <>toSQL "INTO "
                        <>(doubleQuote . safeSQL . tsName . plug2TableSpec $ plug)
                  , toSQL "   ("<>intercalateT (toSQL ", ") (map quotedAttName (plugAttributes plug))<>") "
                  , "VALUES " <> "(" <>valuechain md<> ")" 
                  ]
           valuechain :: [Maybe AAtomValue] -> SQLText
           valuechain record = 
              intercalateT ", " 
                [case att of 
                    Nothing  -> toSQL "NULL" 
                    Just val -> toSQLTxt val 
                | att<-record
                ]

