{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Database.Design.Ampersand.Input.RDF.RDF 
  (parseRdfFile, timBernersLee)
where
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Misc
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Database.Design.Ampersand.Input.ADL1.CtxError
import Database.Design.Ampersand.ADL1
import Database.Design.Ampersand.Core.ParseTree
import Data.RDF

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
    let ts = query rdf (Just (UNode "http://www.w3.org/2011/Talks/0331-hyderabad-tbl/data#talk")) (Just (UNode "dct:title")) Nothing
    let talks = map (\(Triple _ _ (LNode (PlainL s))) -> s) ts
    print rdf
    --print talks
    --print $ baseUrl rdf

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
