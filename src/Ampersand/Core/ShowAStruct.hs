module Ampersand.Core.ShowAStruct
  (AStruct(..))
where

import Ampersand.Basics
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.Core.ShowPStruct
import Ampersand.Core.A2P_Converters
import qualified Data.Text as Text

class AStruct a where
 showA :: a -> String

instance AStruct A_Context where
 showA = showP . aCtx2pCtx

instance AStruct Expression where  
 showA = showP . aExpression2pTermPrim

instance AStruct A_Concept where
 showA = showP . aConcept2pConcept

instance AStruct A_Gen where
 showA = showP . aGen2pGen 

instance AStruct Rule where
 showA = showP . aRule2pRule

instance AStruct Relation where
 showA = showP . aRelation2pRelation

instance AStruct AAtomPair where
 showA p = "("++showA (apLeft p)++","++ showA (apRight p)++")"

instance AStruct AAtomValue where
 showA a = Text.unpack txt
   where 
     ADLText txt = toADLTxt a
instance AStruct ExplObj where
 showA = showP . aExplObj2PRef2Obj

