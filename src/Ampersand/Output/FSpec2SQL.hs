{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2SQL
  (dumpSQLqueries)
where
import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Prototype.Generate 
  (generateDBstructQueries, generateInitialPopQueries
  )
import Ampersand.Core.ShowAStruct
import Ampersand.FSpec
import Ampersand.FSpec.SQL
import Data.Monoid
import qualified Data.Text as Text
import Ampersand.Core.AbstractSyntaxTree
     ( Relation )

dumpSQLqueries :: MultiFSpecs -> SQLText
dumpSQLqueries multi
   = unlinesT $ 
         (header . toSQL . Text.pack $ ampersandVersionStr)
       <>header "Database structure queries"
       <>generateDBstructQueries fSpec True
       <>header "Initial population queries"
       <>map (\q -> q <> ";") (generateInitialPopQueries fSpec)
       <>header "Violations of conjuncts"
       <>concatMap showConjunct (allConjuncts fSpec)
       <>header "Queries per relation"
       <>concatMap showDecl (vrels fSpec)
       <>header "Queries of interfaces"
       <>concatMap showInterface (interfaceS fSpec <> interfaceG fSpec)
    
   where
     fSpec = userFSpec multi
     showInterface :: Interface -> [SQLText]
     showInterface ifc 
        = header ("INTERFACE: "<>(toSQL . Text.pack . name $ ifc))
        <>(map ("  " <>) . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [SQLText]
          showObjDef obj
            = (header . toSQL . Text.pack . showA . objExpression) obj
            <>[(prettySQLQueryWithPlaceholder 2 fSpec . objExpression) obj,";"]
            <>case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            <>header ("Broad query for the object at " <> (toSQL . Text.pack . show . origin) obj)
            <>[prettyBroadQueryWithPlaceholder 2 fSpec $ obj,";"]
          showSubInterface :: SubInterface -> [SQLText]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef . siObjs $ sub
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [SQLText]
     showConjunct conj 
        = (header . toSQL . Text.pack . rc_id $ conj)
        <>[sqlCommnt "Rules for this conjunct:"]
        <>map (sqlCommnt . showRule) (rc_orgRules conj)
        <>[prettySQLQuery 2 fSpec . conjNF (getOpts fSpec) . notCpl . rc_conjunct $ conj,";"]
        where
          showRule r 
            = "  - "<>(toSQL . Text.pack . name $ r)<>": "<>(toSQL . Text.pack . showA $ r)
     sqlCommnt :: SQLText -> SQLText
     sqlCommnt t = "/* "<>t<>" */"
     showDecl :: Relation -> [SQLText]
     showDecl decl 
        = header (toSQL . Text.pack$ showA decl)
        <>[prettySQLQuery 2 fSpec $ decl,";"]
     header :: SQLText -> [SQLText]
     header title = 
         [ ""
         , "/"<>replicateT width "*"<>"/"
         , "/***"<>spaces firstspaces<>title<>spaces (width-6-firstspaces-l)<>"***/"
         , "/"<>replicateT width "*"<>"/"
         , ""
         ]
       where width = maximum [80 , l + 8]
             l = lengthT title
             spaces :: Int -> SQLText
             spaces i = replicateT i " "
             firstspaces :: Int
             firstspaces = (width - 6 - l) `quot` 2 