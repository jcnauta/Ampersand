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
parseRdfFile opts file = undefined


-- | looks up Tim Berners Lee card.rdf file for talks he has given.
--   returns a single String element: [\"Designing the Web for an Open Society\"].
timBernersLee :: IO ()
timBernersLee = do
    Right (rdf::TriplesList) <- parseURL (XmlParser Nothing Nothing) "http://www.w3.org/People/Berners-Lee/card.rdf"
    let ts = query rdf (Just (UNode "http://www.w3.org/2011/Talks/0331-hyderabad-tbl/data#talk")) (Just (UNode "dct:title")) Nothing
    let talks = map (\(Triple _ _ (LNode (PlainL s))) -> s) ts
    print rdf
    print talks
    print $ baseUrl rdf
