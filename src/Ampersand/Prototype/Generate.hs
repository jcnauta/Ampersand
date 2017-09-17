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


generateDBstructQueries :: FSpec -> Bool -> [Text.Text]
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
generateInitialPopQueries :: FSpec -> [Text.Text]
generateInitialPopQueries fSpec 
  = fillSignalTable (initialConjunctSignals fSpec) <>
    populateTablesWithPops False fSpec
  where
    fillSignalTable :: [(Conjunct, [AAtomPair])] -> [Text.Text]
    fillSignalTable [] = []
    fillSignalTable conjSignals 
     = [Text.unlines
            [ "INSERT INTO "<>(safeSQLObjectName . Text.pack . tableName $ signalTableSpec)
            , "   ("<>Text.intercalate ", " (map (safeSQLObjectName . Text.pack) (attributeNames signalTableSpec))<>")"
            , "VALUES " <> Text.intercalate " , " 
                  [ "(" <>Text.intercalate ", " [(Text.pack . safeSQLLiteral . rc_id $ conj)
                                                ,(Text.pack . safeSQLLiteral . showValSQL . apLeft  $ p)
                                                ,(Text.pack . safeSQLLiteral . showValSQL . apRight $ p)
                                                ]<> ")" 
                  | (conj, viols) <- conjSignals
                  , p <- viols
                  ]
            ]
       ]
generateMetaPopQueries :: FSpec -> [Text.Text]
generateMetaPopQueries = populateTablesWithPops True

populateTablesWithPops :: Bool -> FSpec -> [Text.Text]
populateTablesWithPops ignoreDoubles fSpec =
      concatMap populatePlug [p | InternalPlug p <- plugInfos fSpec]
      where
        populatePlug :: PlugSQL -> [Text.Text]
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
           insertRecord md = 
                Text.unlines
                  [ "INSERT "<> (if ignoreDoubles then "IGNORE " else "") <>"INTO "
                        <>Text.pack (show (name plug))
                  , "   ("<>Text.intercalate ", " (map (safeSQLObjectName . Text.pack . attName) (plugAttributes plug))<>") "
                  , "VALUES " <> "(" <>valuechain md<> ")" 
                  ]
           valuechain record 
             = Text.intercalate ", " 
                 [case att of 
                    Nothing -> "NULL"
                    Just val -> Text.pack . safeSQLLiteral $ showValSQL val
                 | att <- record ]

