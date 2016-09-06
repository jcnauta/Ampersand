{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ampersand.Input.RDF.RDF 
  (parseRdfFile, timBernersLee)
where
import Ampersand.Basics
import Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Ampersand.Input.ADL1.CtxError
import Ampersand.ADL1
import Data.RDF
import Data.List
import Data.Text as T (Text,unpack, pack)

parseRdfFile :: Options 
              -> FilePath -> IO (Guarded [P_Context])
parseRdfFile opts file = do 
    result <- parseFile (XmlParser Nothing Nothing) file
    case result of 
      Left (ParseFailure msg)  -> return $ mkRDFParseError file msg
      Right (rdf::TriplesList) -> return (pure [rdf2pContext file rdf])



-- | looks up Tim Berners Lee card.rdf file for talks he has given.
--   returns a single String element: [\"Designing the Web for an Open Society\"].
timBernersLee :: IO ()
timBernersLee = do
    Right (rdf::TriplesList) <- parseFile (XmlParser Nothing Nothing) "testdata.rdf"
    let members =  filter (tripleContainsNode memberNode) . triplesOf $ rdf
    mapM_ print . take 200 . uniqueRelsOf $ rdf
    mapM_ print members
    putStrLn $ "#Triples: "++ (show . length . triplesOf $ rdf)
    putStrLn $ "#Rels: "++ (show . length . uniqueRelsOf $ rdf)
    putStrLn $ "#Members: "++ (show . length $ members)
    mapM_ print . nub . objectsOfPredicate rdf $ memberNode
     
memberNode = UNode . T.pack $ "rdfs:member"
uniqueRelsOf :: RDF a => a -> [TestRel]
uniqueRelsOf rdf =  nub . map rdfRelType $ triplesOf rdf
  where 
    rdfRelType :: Triple -> TestRel
    rdfRelType t 
      = TestRel 
         (node2String . predicateOf  $ t)  -- name
         (sourceOf t)     -- source
         (targetOf t)     -- target
         isLit                          -- is it univalent? When the target is a literal, we assume that the relation is univalent.
      where
       sourceOf :: Triple -> TestCpt
       sourceOf = cptOfNode . subjectOf
       targetOf :: Triple -> TestCpt
       targetOf = cptOfNode . objectOf

       isLit :: Bool
       isLit = case objectOf t of
                    LNode _ -> True
                    _ -> False
       cptOfNode :: Node -> TestCpt
       cptOfNode n = 
         case n of 
           LNode (PlainL _)     -> TestCpt litType
           LNode (PlainLL _ _)  -> TestCpt litType
           LNode (TypedL lit typ) -> fatal 45 $ "Interesting! `" ++T.unpack lit ++"` is of type `"++T.unpack typ++"`"
           UNode x 
             | isMember x -> findMember . objectOf  $ t
             | otherwise  -> TestCpt $ "TODO: "++ show n 
           _ -> TestCpt $ "xxx-"++node2String n
        where 
          litType = case predicateOf t of
                      UNode x -> T.unpack x 
                      _  -> fatal 54 "Unexpected node type."
          isMember :: T.Text -> Bool
          isMember txt =  txt == T.pack "rdfs:member"
          
            
          findMember :: Node -> TestCpt
          findMember = undefined
node2String :: Node -> String 
node2String n = 
  case n of 
    UNode t                -> "UNode: "++T.unpack t
    BNode t                -> T.unpack t
    BNodeGen i             -> show i
    LNode (PlainL lit)     -> "Lit:"++T.unpack lit
    LNode (PlainLL lit _)  -> "Lit:"++T.unpack lit
    LNode (TypedL lit typ) -> fatal 45 $ "Interesting! `" ++T.unpack lit ++"` is of type `"++T.unpack typ++"`"

data TestRel = TestRel String TestCpt TestCpt Bool -- Name, Source, Target, Univalent?
  deriving Eq
instance Show TestRel where
  showsPrec _ (TestRel n s t pUni)
   = showString . unlines $
       [ "name:   "++n++if pUni then " [UNI]" else ""
       , "source: "++show s
       , "target: "++show t
       ]
data TestCpt = TestCpt String
  deriving Eq
instance Show TestCpt where
  showsPrec _ (TestCpt x)
   = showString x  

rdf2pContext :: RDF a => String -> a -> P_Context
rdf2pContext nm rdf  
   = PCtx{ ctx_nm     = nm
         , ctx_pos    = []
         , ctx_lang   = Dutch
         , ctx_markup = Nothing
         , ctx_thms   = []
         , ctx_pats   = []
         , ctx_rs     = []
         , ctx_ds     = []
         , ctx_cs     = []
         , ctx_ks     = []
         , ctx_rrules = []
         , ctx_rrels  = []
         , ctx_reprs  = []
         , ctx_vs     = []
         , ctx_gs     = []
         , ctx_ifcs   = []
         , ctx_ps     = []
         , ctx_pops   = []
         , ctx_sql    = []
         , ctx_php    = []
         , ctx_metas  = []
         } 
