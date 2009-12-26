{-# OPTIONS_GHC -Wall #-}
module Adl.Rule    ( Rule(..),Rules
                   , RuleType(..)
                   , consequent, antecedent, ruleType, normExpr, multRules, rulefromProp, isaRule)     
where
   import Adl.FilePos                   ( FilePos(..),Numbered(..))
   import Adl.Concept                   ( Concept(..)
                                        , Association(..)
                                        , MorphicId(..),Morphic(..))
   import Adl.MorphismAndDeclaration    ( Morphism(..),Declaration(..),mIs)
   import Adl.Expression                ( Expression(..),v)
   import Adl.Prop
   import CommonClasses                 ( Identified(name,typ)
                                        , Explained(explain))
                                           
   type Rules = [Rule]
   data Rule =
  -- Ru c antc p cons expla sgn nr pn
        Ru { rrsrt :: RuleType          -- ^ One of the following:
                                        --    | Implication if this is an implication;
                                        --    | Equivalence if this is an equivalence;
                                        --    | Truth  if this is an ALWAYS expression.
           , rrant :: Expression        -- ^ Antecedent
           , rrfps :: FilePos           -- ^ Position in the ADL file
           , rrcon :: Expression        -- ^ Consequent
--           , r_cpu :: Expressions       -- ^ This is a list of subexpressions, which must be computed.
           , rrxpl :: String            -- ^ Explanation
           , rrtyp :: (Concept,Concept) -- ^ Sign of this rule
           , rrdcl :: Maybe (Prop,Declaration)  -- ^ The property, if this rule originates from a property on a Declaration
           , runum :: Int               -- ^ Rule number
           , r_pat :: String            -- ^ Name of pattern in which it was defined.
           , r_usr :: Bool              -- ^ True if this rule was specified explicitly as a rule in the ADL-script; False if it follows implicitly from the ADL-script and generated by a computer
           }
  -- Sg p rule expla sgn nr pn signal
      | Sg { srfps :: FilePos           -- ^ position in the ADL file
           , srsig :: Rule              -- ^ the rule to be signalled
           , srxpl :: String            -- ^ explanation
           , srtyp :: (Concept,Concept) -- ^ type
           , runum :: Int               -- ^ rule number
           , r_pat :: String            -- ^ name of pattern in which it was defined.
           , srrel :: Declaration       -- ^ the signal relation
           } deriving (Eq)
   data RuleType = Implication | Equivalence | Truth | Generalization | Automatic deriving (Eq,Show)

   isaRule :: Rule -> Bool    -- tells whether this rule was declared as an ISA rule
   isaRule Ru{rrfps=FilePos(_,_,str)} = str == "ISA"
   isaRule _ = False

   instance Show Rule where
    showsPrec _ x =
       case x of
          Ru{rrsrt = Implication   } -> showString$ show(rrant x) ++ " |- " ++ (show$rrcon x)
          Ru{rrsrt = Equivalence   } -> showString$ show(rrant x) ++ " = "  ++ (show$rrcon x)
          Ru{rrsrt = Truth         } -> showString$ show(rrcon x)
          Ru{rrsrt = Automatic     } -> showString ""
          Ru{rrsrt = Generalization} -> showString ""
          Sg{}                       -> showString$ "SIGNAL: " ++ (show$srsig x)
        
   instance Numbered Rule where
    pos r = case r of
              Ru{}  ->  rrfps r
              Sg{}  ->  srfps r
    nr r = runum r

      
   instance Identified Rule where
    name r = "Rule"++show (runum r)
    typ _ = "Rule_"
    
   instance Association Rule where
    source r  = case r of
                  Ru{} -> fst (rrtyp r)
                  Sg{} -> fst (srtyp r)
    target r  = case r of
                  Ru{} -> snd (rrtyp r)
                  Sg{} -> snd (srtyp r)

   instance Explained Rule where
    explain _ r = case r of         -- TODO: to allow explainations in multiple languages, change to:  explain options d@Sgn{} = etc...
                   Ru{}  ->  rrxpl r
                   Sg{}  ->  srxpl r
                   _     ->  ""

   instance MorphicId Rule where
    isIdent r = isIdent (normExpr r)

   instance Morphic Rule where
    multiplicities _  = []
    isMph r = case r of
                Ru{rrsrt=Truth} -> isMph (rrcon r)
                Sg{}            -> isMph (srsig r)
                _               -> False
                
--    isMph r  | ruleType r==Truth = isMph (consequent r)
--             | otherwise       = False
    flp r = case r of
                Ru{} -> r{rrant = if rrsrt r == Truth
                                  then error ("!Fatal (module Rule 110): illegal call to antecedent in flp ("++show r++")")
                                  else flp (rrant r)
                         ,rrcon = flp (rrcon r)
                         ,rrtyp = (target (rrtyp r),source (rrtyp r))
                         }
                _    -> error ("!Fatal (module Rule 115): flp undefined for rule:\n "++show r)
  --  isIdent r = error ("!Fatal (module Rule 116): isIdent not applicable to any rule:\n "++showHS "" r)
    typeUniq r | ruleType r==Truth = typeUniq (antecedent r)
               | otherwise       = typeUniq (antecedent r) && typeUniq (consequent r)
--    isIdent r = isIdent (normExpr r)
    isProp r = isProp (normExpr r)
    isTrue r | ruleType r==Truth  = isTrue (consequent r)
             | otherwise        = isTrue (consequent r) || isFalse (consequent r)
    isFalse r| ruleType r==Truth  = isFalse (consequent r)
             | otherwise        = isFalse (consequent r) && isTrue (consequent r)
    isSignal r = case r of
                   Sg{} -> True
                   _    -> False 
    isNot r  | ruleType r==Truth  = isNot (consequent r)
             | otherwise        = False  -- TODO: check correctness!

   normExpr :: Rule -> Expression
   normExpr rule
    | isSignal rule      = v (sign rule)
    | ruleType rule==Truth = consequent rule
    | ruleType rule==Implication = Fu [Cp (antecedent rule), consequent rule]
    | ruleType rule==Equivalence = Fi [ Fu [antecedent rule, Cp (consequent rule)]
                              , Fu [Cp (antecedent rule), consequent rule]]
    | otherwise          = error("!Fatal (module Rule 138): Cannot make an expression of "++show rule)

   ruleType :: Rule -> RuleType
   ruleType r = case r of 
                   Ru{} -> rrsrt r
                   Sg{} -> ruleType (srsig r)

   antecedent :: Rule -> Expression
   antecedent r = case r of
                   Ru{rrsrt = Truth} -> error ("!Fatal (module Rule 148): illegal call to antecedent of rule "++show r)
                   Ru{} -> rrant r
                   Sg{} -> antecedent (srsig r)
                   
   consequent :: Rule -> Expression
   consequent r = case r of
                   Ru{} -> rrcon r
                   Sg{} -> consequent (srsig r)

   multRules :: Declaration -> [Rule]
   multRules d
     = [rulefromProp p d | p<-multiplicities d, p `elem` [Uni,Tot,Inj,Sur,Sym,Asy,Trn,Rfx]
                         , if source d==target d || p `elem` [Uni,Tot,Inj,Sur] then True else
                           error ("!Fatal (module Rule 163): Property "++show p++" requires equal source and target domains (you specified "++name (source d)++" and "++name (target d)++").") ]
 
   rulefromProp :: Prop -> Declaration -> Rule
   rulefromProp prp d@(Sgn{})
      = Ru { rrsrt = case prp of
                        Uni-> Implication
                        Tot-> Implication
                        Inj-> Implication
                        Sur-> Implication
                        Sym-> Equivalence
                        Asy-> Implication
                        Trn-> Implication
                        Rfx-> Implication
                        Aut->  error $ "!Fatal (module Rule 176): There is no rule for this prop."
           , rrant = case prp of
                        Uni-> F [flp r,r] 
                        Tot-> i
                        Inj-> F [r,flp r]
                        Sur-> i
                        Sym-> r
                        Asy-> Fi [flp r,r]
                        Trn-> F [r,r]
                        Rfx-> i 
                        Aut->  error $ "!Fatal (module Rule 186): There is no rule for this prop."
           , rrfps = pos d
           , rrcon = case prp of
                        Uni-> i
                        Tot-> F [r,flp r]
                        Inj-> i
                        Sur-> F [flp r,r]
                        Sym-> flp r
                        Asy-> i
                        Trn-> r
                        Rfx-> r
                        Aut->  error $ "!Fatal (module Rule 197): There is no rule for this prop."
           
           , rrxpl = case prp of
                        Sym-> name d++"["++name (source d)++"*"++name (source d)++"] is symmetric."    
                        Asy-> name d++"["++name (source d)++"*"++name (source d)++"] is antisymmetric."
                        Trn-> name d++"["++name (source d)++"*"++name (source d)++"] is transitive."
                        Rfx-> name d++"["++name (source d)++"*"++name (source d)++"] is reflexive."
                        Uni-> name d++"["++name (source d)++"*"++name (target d)++"] is univalent"
                        Sur-> name d++"["++name (source d)++"*"++name (target d)++"] is surjective"
                        Inj-> name d++"["++name (source d)++"*"++name (target d)++"] is injective"
                        Tot-> name d++"["++name (source d)++"*"++name (target d)++"] is total"
                        Aut-> error ("!Fatal (module Rule 208): rulefromProp cannot explain "++show prp)
           , rrtyp = (Anything,Anything)  -- The type checker will assign the type
           , rrdcl = (Just (prp,d))       -- For traceability: The original property and declaration.
           , runum = 0                    -- Rules will be renumbered after enriching the context
           , r_pat = decpat d             -- For traceability: The name of the pattern. Unknown at this position but it may be changed by the environment.
           , r_usr = False                
           }
          where
           i = Tm $ mIs Anything
           r = Tm $ Mph (name d)  (pos d) [source d,target d] (source d,target d) True d 
   rulefromProp _ _ = error ("!Fatal (module Rule 218): Properties can only be set on user-defined Declarations.")
    
