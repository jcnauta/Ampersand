  {-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances #-}
  module Calc ( ComputeRule(CR)
              , deriveProofs
              , triggers
              , disjNF
              , conjNF
              , homogeneous
              , computeOrder
              , allClauses
              , lClause
              , rClause
              , conjuncts
              , makeRule
              , doClause
              , simplPAclause
              , informalRule ) 
  where 
   import Char ( isSpace )
   import CommonClasses (Identified(name) )
   import Collection (Collection (uni,isc,empty,rd))
   import Auxiliaries
   import Classification
   import ADLdef
   import FspecDef
   import ShowADL
   import ShowHS
   import CC_aux
   import Graphic ( dotGraph
                  , processDotgraphFile
                  , Graphic)


   multiplicityAnalysis context 
    = let fnm = "MULT"++name context in
      putStr ("\n--------------\n"++
              "Multiplicity Analysis:\n")>>
      writeFile (fnm++".dot") (dotGraph context "anyStyle" fnm (multDerivations context)) >>
      putStr ("\nMultiplicity propagation graph "++fnm++".dot written... ") >>
      processDotgraphFile fnm >>
      writeFile ("CF"++fnm++".dot") (dotGraph context "anyStyle" fnm codeFragments) >>
      putStr ("\nCode graph CF"++fnm++".dot written... ") >>
      processDotgraphFile ("CF"++fnm)
      where
       codeFragments :: [ECArule]
       codeFragments = [ eca | rule<-declaredRules context, clause<-conjuncts rule, eca<-doClause (simplify clause) ]

   deriveProofs context
    = "\nSignals for "++name context++"\n--------------\n"++
      proof (signals context)++
      "\nRules for "++name context++"\n--------------\n"++
      proof (declaredRules context)++
      "\n--------------\n"++
      "Summarizing all compute rules: \n  "++
      chain "\n  " [ informalRule {-(declarations frExpr)-} hc | rule<-declaredRules context, hc@(CR (fOps, e, bOp, toExpr, frExpr, rule))<-triggers rule]++
      "\n--------------\n"++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      "Ontological analysis: \n  "++
      chain "\n\n  " [name o++"("++chain ", " [name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):\n  "
                     | o<-attributes context, c<-[concept o]]++
      "\n--------------\n"++
      "Triggers from objects: \n     "++
      chain "\n     " [name c++"("++chain ", "[name a++"["++(name.target.ctx) a++"]"|a<-attributes o]++"):"++
                       condNull ("\n  Rules for Insert transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "INSERT INTO" (Isn c c:map makeDeclaration (mors o)))++          -- taken from phpCodeEntCreate
                       condNull ("\n  Rules for Update transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "UPDATE"              (map makeDeclaration (mors o)))++          -- taken from phpCodeEntUpdate
                       condNull ("\n  Rules for Delete transactions\n    ") (chain "\n    ") informalRule 
                        (computeOrder hcs "DELETE FROM" (Isn c c:map makeDeclaration (mors o)))++          -- taken from phpCodeEntDelete
                       "\n"
                      | o<-attributes context, c<-[concept o]]++
      "\n--------------\n"
      where
       hcs = [hc| rule<-declaredRules context++multRules context, hc<-triggers rule ]
       sh x = showHS "" x

   condNull header fold f xs = if null xs then "" else header++fold (map f xs)




   delMors :: Context -> Concept -> [Morphism]
   delMors context e = [m| m<-rd (ms++[ m| m<-rd (ms'++map flp ms'), sur (multiplicities m)]), source m == e]
    where ms' = mors (rules context)
          ms = rd [ if null (morlist term) then error "Module Calc: head error 1" else
                    head (morlist term)
                  | rule<-rules context
                  , conjunct<-conjuncts rule
                  , Fu terms<-ilClauses conjunct
                  , F ts<-terms, ts'<-[[t| t<-ts, length (morlist t)==1]]
                  , if null ts' then error(" module Calc "++show (nr rule)++" ("++show (pos rule)++") in "++showADL rule++"\nterms = "++showHS "" (Fu terms)++"\nts = "++showHS "" (F ts)) else True
                  , term<-[flp (head ts'), last ts']--, term `elem` r_cpu rule
                  ]

   delFrs :: Context -> Concept -> [Rule]
   delFrs context e
        = [ makeRule rule (Fu terms)
          | rule<-rules context
          , conjunct<-conjuncts rule
          , clause@(Fu terms)<-allClauses conjunct
          , and [idsOnly t| Cp t<-terms], source clause==e]

   proof :: [Rule] -> String
   proof rs
    = chain "\n--------------\n"
      [ (if isSignal rule then "SIGNAL\n" else "")++
        chain "\n" ["   "++stmt++if null comment then "" else "\n<=> { "++comment++". }"
                   | (stmt,comment)<-cleanup (derivation rule)]
      | rule<-rs]
      where derivation rule@(Ru rt _ _ _ _ _ _ _ _)
             = [ (showADL rule          , if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
               , (showProof prf         , "")
               ]++
               if rt==AlwaysExpr then [] else
               [ ("\nViolations are computed by (conjNF . Cp . normexpr) rule:\n     "++
                  (showProof.cfProof. Cp . normExpr) rule++"\n"
                 , "")
               , ("\nConjuncts:\n     "++
                  chain "\n     " (rd[showADL conjunct
                                     |conjunct<-conjuncts rule])
                 , "")
               , ("\nClauses:\n     "++
                  chain "\n     " (rd[showADL (makeRule rule clause)
                                     |conjunct<-conjuncts rule, clause<-allClauses conjunct])
                 , "")
               , ("\nniClauses:\n     "++
                  chain "\n     " (map (showHS "") (rd [clause|conjunct<-conjuncts rule, clause<-niClauses conjunct]))
                 , "")
               , ( "\nAvailable Triggers on rule "++show (nr rule)++":\n     "++
                   chain "\n     " [showADL (makeRule rule clause)++ " yields"++concat
                                    [ "\n        "++informalRule {-(declarations conjunct)-} hc
                                    | hc<-hornCs rule clause
                                    ]
                                   |conjunct<-conjuncts rule, clause<-allClauses conjunct]++
                   "\nAvailable code fragments on rule "++show (nr rule)++":\n     "++
                   chain "\n     " [showADL (makeRule rule r)++ " yields\n"++chain "\n\n"
                                    [ "event = "++show ev++" "++showADL m++"\n"++
                                      showADL r++"["++showADL m++":="++showADL (actSem ev (Tm m) (delta (sign m)))++"] = r'\n"++
                                      "r'    = "++(showProof.cfProof) r'++"\n"++
                                      "viols = r'-"++(showProof.cfProof) (Cp r')++"\n"++
                                --      "reaction? evaluate r -: r' ("++(showADL.conjNF) (Fu[Cp r,r'])++")"++
                                --         (showProof.cfProof) (Fu[Cp r,r'])++"\n"++
                                --      "delta: r-/\\r' = "++(showProof.cfProof) (Fi[notCp r,r'])++
                                --      "\nNow compute a reaction\n(isTrue.conjNF) (Fu[Cp r,r']) = "++show ((isTrue.conjNF) (Fu[Cp r,r']))++"\n"++
                                      (if null (lambda ev (Tm m) r)
                                       then "lambda "++showADL m++" ("++showADL r++") = empty\n"
                                       else {- for debug purposes:
                                               "lambda "++show ev++" "++showADL m++" ("++showADL r++") = \n"++(chain "\n\n".map showProof.lambda ev (Tm m)) r++"\n"++
                                               "derivMono ("++showADL r++") "++show ev++" "++showADL m++"\n = "++({-chain "\n". map -}showProof.derivMono r ev) m++"\n"++
                                               "\nNow compute checkMono r ev m = \n"++show (checkMono r ev m)++"\n"++ -}
                                            if (isTrue.conjNF) (Fu[Cp r,r'])
                                            then "A reaction is not required, because  r -: r'. Proof:"++(showProof.cfProof) (Fu[Cp r,r'])++"\n"
                                            else if checkMono r ev m
                                            then "A reaction is not required, because  r -: r'. Proof:"++(showProof.derivMono r ev) m++"\n"
                                            else "The correct reaction on this event is\n"++show (ECA (On ev m) (doCode viols Ins r'))++"\n"++
                                                 "\ndoClause :\n"++show (ECA (On ev m) (doCode (Fi [Cp r,nr']) Ins r))
                                      )
                                    | m<-rd [m|x<-mors r, m<-[x,flp x], inline m, not (isIdent m)] -- TODO: include proofs that allow: isIdent m
                                    , ev<-[Ins,Del]
                                    , r'<-[subst (Tm m,actSem ev (Tm m) (delta (sign m))) r]
                                    , nr'<-[conjNF r']
                                    , viols<-[conjNF (Cp r')]
                                    , True ]  -- (isTrue.conjNF) (Fu[Cp r,r'])
                                   |conjunct<-conjuncts rule, r<-allClauses conjunct]
                 , "")
               , ("\nGenerated Triggers for: "++showADL rule++" (rule "++show (nr rule)++")\n     "++
                   chain "\n     " ([ informalRule {-(declarations frExpr)-} hc | hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-triggers rule]), "")
               ] where prf = nfProof (normExpr rule)
                       cfProof expr = nfPr True False (simplify expr)
                       dfProof expr = nfPr True True (simplify expr)
                       checkMono expr ev m = simplify expr == simplify (antecedent conclusion) &&
                                             simplify (subst (Tm m,actSem ev (Tm m) (delta (sign m))) expr) == simplify (consequent conclusion)
                         where (conclusion,_,_) = last (derivMono expr ev m)
            derivation r@(Sg p rule expla sgn _ pn signal)
             = [ (showADL r          , if null prf then "Translates directly to conjunctive normal form" else "Convert into conjunctive normal form")
               , (showProof prf      , "")
               ]++
               [ ("\nConjuncts:\n     "++
                  chain "\n     " (rd[showADL conjunct
                                     |conjunct<-conjuncts rule])
                 , "")
               , ("\nClauses:\n     "++
                  chain "\n     " (rd[showADL (makeRule rule clause)
                                     |conjunct<-conjuncts rule, clause<-allClauses conjunct])
                 , "")
               ] where prf = nfProof (normExpr rule)

 --         dummy = Fu [Cp (F [Tm (Mph "q" posNone [] (C "A" (==) [],C "C" (==) []) True dec) ])
 --                    , F [Tm (Mph "r" posNone [] (C "A" (==) [],C "B" (==) []) True dec)
 --                        ,Tm (Mph "s" posNone [] (C "B" (==) [],C "C" (==) []) True dec)
 --                        ]
 --                    ]
 --                 where dec = error ("(Module Calc) Declaration error")
            cleanup :: [(String,String)] -> [(String,String)]
            cleanup [x] = [x]
            cleanup ((x,c):(x',c'):xs) = if x==x' then rest else (x,c): rest where rest = cleanup ((x',c'):xs)
            cleanup [] = []
            sh (Fi []) = "empty"
            sh f       = showADL f
            showcomp rule clause -- laat de functionaliteit van 'computing' in de functie 'triggers' zien (nuttig voor testen).
             = [ "\ntoExpr                  : "++showADL toExpr++
                 "\nr_cpu rule              : "++show (r_cpu rule)++
                 "\ntoExpr `elem` r_cpu rule: "++show ((map name.morlist) toExpr `elem` map (map name.morlist) (r_cpu rule))
               | (CR (fOps, e, bOp, toExpr, frExpr, r))<-hornCs rule clause ]

   doClause :: Expression -> [ECArule]
   doClause r
    = {- if error("Diagnostic: \n"++
               showADL r++"\n"++
               showHS "   " r++"\n"++
               "mors r = "++show (mors r)++"\n"++
               "subst r =\n   "++chain "\n   " ([ "m="++showADL m++"\nsubst ("++showADL m++","++showADL (Fu[m,delta (sign m)])++") ("++showADL r++") = "++showADL (subst (m,Fu[m,delta (sign m)]) r)++" <=> (r') "++showADL r'++"\n phi: "++showADL phi++" <=> "++(showADL.conjNF) phi++"\n (Fu[Cp r,r']): "++showADL (Fu[Cp r,r'])++" <=> "++disjNF (Fu[Cp r,r'])
                                                | m <-rd [Tm m|x<-mors r, m<-[x,flp x], inline m]
                                                , r'<-[(conjNF.subst (m,Fu[m,delta (sign m)])) r]
                                                , phi<-[Fi[Cp r',r]]
                                                ])++"\n"
              ) then res else -} res
    where
     disjNF  expr = negRight (if null proof then expr else expr')
                    where (expr',motives,equ) = last proof
                          proof = nfPr True  True expr
     res = -- [ ECA (On Del (V [] (sign to))) (doCode (Cp r) Ins to) | null [t| t<-ts, isNeg t],  to<-[t| t<-ts, isPos t]]++
           [ ECA (On ev m) (doCode phi Ins r)
           | m<-rd [m|x<-mors r, m<-[x,flp x], inline m]
           , ev<-[Ins,Del]
           , r'<-[(conjNF.subst (Tm m,actSem ev (Tm m) (delta (sign m))) ) r]
           , viols<-[conjNF (Cp r')]
           , phi<-[(Fi [Cp r,r'])]
        {- , (not.isTrue) r' -} ]

   actSem Ins e delta = Fu[e,delta]
   actSem Del e delta = Fi[e,Cp delta]

   makeTm name = Tm (makeMph (Sgn name cptAnything cptAnything [] "" "" "" [] "" posNone 0 True))
   delta (a,b) = Tm (makeMph (Sgn "Delta" a b [] "" "" "" [] "" posNone 0 True))

-- TODO: De volgende code voor simplPAclause stinkt. Opzoeken: procesalgebra herschrijfregels.

   simplPAclause :: PAclause -> PAclause
   simplPAclause (Choice [c])             = simplPAclause c
   simplPAclause (Choice [])              = Choice []
-- ?   simplPAclause (Choice (All []:cs))  = simplPAclause (Choice cs) -- TODO: wat moet er in dit geval gebeuren?
   simplPAclause (Choice (Choice cs:cs')) = simplPAclause (Choice (cs++cs'))
   simplPAclause (Choice (c:cs))          = f (simplPAclause (Choice cs))
                                            where f (Choice [])  = Choice []
                                                  f (Choice cs') = Choice (simplPAclause c:cs')
                                                  f cl           = error ("Module Calc: something funny in simplPAclause (Choice ("++show c++":"++show cs++"))")
   simplPAclause (All [c])                = simplPAclause c
   simplPAclause (All [])                 = All []
   simplPAclause (All (Choice []:cs'))    = Choice []
   simplPAclause (All (All cs:cs'))       = simplPAclause (All (cs++cs'))
   simplPAclause (All (c:cs))             = f (simplPAclause (All cs))
                                            where f (Choice []) = Choice []
                                                  f (All cs')   = All (c:cs')
                                                  f cl          = error ("Module Calc: something funny in simplPAclause (All ("++show c++":"++show cs++"))")
   simplPAclause c                        = c

-- | de functie doCode beschrijft de voornaamste mogelijkheden om een expressie delta' te verwerken in expr (met tOp'==Ins of tOp==Del)

   doCode delta' tOp' expr = {- simplPAclause -} (doCod delta' tOp' expr)
    where
      doCod delta tOp (Fu []) = error ("!Fail (Module Calc): doCod ("++showADL delta++") "++show tOp++" "++showADL (Fu [])++",\n"++
                                       "within function doCode ("++showADL delta'++") "++show tOp'++" ("++showHS "" (F [])++").")
      doCod delta tOp (Fi []) = error ("!Fail (Module Calc): doCod ("++showADL delta++") "++show tOp++" "++showADL (Fi [])++",\n"++
                                       "within function doCode ("++showADL delta'++") "++show tOp'++" ("++showHS "" (F [])++").")
      doCod delta tOp (F [])  = error ("!Fail (Module Calc): doCod ("++showADL delta++") "++show tOp++" "++showADL (F [])++",\n"++
                                       "within function doCode ("++showADL delta'++") "++show tOp'++" ("++showHS "" (F [])++").")
      doCod delta tOp (F [t]) = doCod delta Ins t

      doCod delta Ins (Cp x ) = doCod delta Del x
      doCod delta Ins (Fu fs) = Choice [ doCod delta Ins f | f<-fs ]
      doCod delta Ins (Fi fs) = All    [ doCod delta Ins f | f<-fs ]
      doCod delta Ins f@(F ts)
       = All [ c | (l,r)<-chop ts 
                 , one <- [Tm (Mph "One" posNone [] (source (F r),target (F l)) True
                                   (Sgn "One" (target (F l)) (source (F r)) [Sym,Asy,Trn] "" "" "" [] "" posNone 0 True))]
                 , c<-[ Do Ins (F l) (simplify (Fi [F[delta',v (target delta,target one)],F[v(source delta,source one),one]]))
                      , Do Ins (F r) (simplify (Fi [F[one,v(target one,target delta)],F[v (source one,source delta),delta']]))
                      ]
                 ]
         where
            delta' = Fi[delta,Cp f]
      doCod delta Ins (Tm m)  = Do Ins (Tm m) delta
      doCod delta Del (Cp x ) = Choice [ doCod delta Ins x
                                       , doCod (F [delta, v (target x,source x)]) Del (Tm (mIs (source x)))
                                       , doCod (F [v (source x,target x), flp delta]) Del (Tm (mIs (target x)))
                                       ]
      doCod delta Del (Fu fs) = All    [ doCod delta Del f | f<-fs ]
      doCod delta Del (Fi fs) = Choice [ doCod delta Del f | f<-fs ]
      doCod delta Del (F ts)
       = Choice [ All [ Do Del (F l) (Fd [ F[delta, Cp (flp (F r))]])
                      , Do Del (F r) (Fd [ F[Cp (flp (F l)), delta]])
                      ]
                | (l,r)<-chop ts ]
      doCod delta Del (Tm m)  = Do Del (Tm m) delta
      doCod delta tOp (Fd ts) = doCod delta tOp (Cp (F (map Cp ts)))
      doCod delta tOp (K0 x ) = doCod (deltaK0 delta tOp x) tOp x
      doCod delta tOp (K1 x ) = doCod (deltaK1 delta tOp x) tOp x
{-
      doCod delta tOp (Tm m)  = if name m=="Delta" then Choice [] else
                                if name m=="One"
                                then New (source m)
                                else if tOp==Ins 
                                     then Do Ins (Tm m) (f Ins (conjNF (Fi [Cp (Tm m),delta])))
                                     else Do Del (Tm m) (f Del (conjNF (Fi [    Tm m ,delta])))
                                where -- De functie f versimpelt de uitdrukking (en dus de SELECT expressie), maar nu moet wel INSERT IGNORE gebruikt worden (DELETE is al IGNORE)
                                  f Ins (Fi fs) = simplify (Fi [f| f<-fs, not (isNeg f && notCp f==Tm m)])
                                  f Del (Fi fs) = simplify (Fi [f| f<-fs, not (isPos f &&       (f==Tm m || (isIdent f && isIdent m)) )])
                                  f _ e = e
-}
      doCod delta tOp e = error ("!Fail (Module Calc): Non-exhaustive patterns in the recursive call doCod ("++showADL delta++") "++show tOp++" ("++showHS "" e++"),\n"++
                                 "within function doCode ("++showADL delta'++") "++show tOp'++" ("++showHS "" expr++").")

   chop [x]    = []
   chop (x:xs) = ([x],xs): [(x:l, r)| (l,r)<-chop xs]
   chop []     = []

   deltaK0 delta Ins x = delta  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta -: x*)
   deltaK0 delta Del x = delta  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x* leeg is)
   deltaK1 delta Ins x = delta  -- error! (tijdelijk... moet berekenen welke paren in x gezet moeten worden zodat delta -: x+)
   deltaK1 delta Del x = delta  -- error! (tijdelijk... moet berekenen welke paren uit x verwijderd moeten worden zodat delta/\x+ leeg is)

   dos (Choice ds) = concat (map dos ds)
   dos (All ds) = concat (map dos ds)
   dos x = [x]

   fragmentProof clause (ECA (On fOp frm) (Do dOp toExpr phi))
    = (clause',[motivate fOp (delta (sign frm)) frm],""):
      (clause'',[motivate dOp ((conjNF. Cp) clause) toExpr],""):
      (definiens,["Now prove that precondition -: postcondition = V"],""):
      nfp'
      where
        nfp' = -- if (not.isTrue.last) [c|(c,m,e)<-nfp]
               -- then error("Clause: "++showADL clause++"\nThe derivation of \"precondition -: postcondition\" should yield V,\n but instead:"++showProof nfp) else
               nfp
        nfp  = nfProof definiens
        clause'  = subst (Tm frm, actSem fOp (Tm frm) (delta (sign frm))) clause
        clause'' = subst (toExpr, actSem dOp toExpr phi) clause'
        definiens = Fu [Cp clause, clause'']
        motivate Ins delta m = "INSERT "++showADL delta++" INTO ("++showADL m++");"
        motivate Del delta m = "DELETE "++showADL delta++" FROM ("++showADL m++");"





   conjuncts :: Rule -> [Expression]
   conjuncts = fiRule.conjNF.normExpr
    where fiRule (Fi fis) = {- map disjuncts -} fis
          fiRule r        = [ {- disjuncts -} r]

   disjuncts :: Expression -> Expression
   disjuncts = fuRule
    where fuRule (Fu cps) = (Fu . rd . map cpRule) cps
          fuRule r        = Fu [cpRule r]
          cpRule (Cp r)   = Cp (fRule r)
          cpRule r        = fRule r
          fRule (F ts)    = F ts
          fRule  r        = F [r]





   allClauses cl = rd [simplify e|e<-shiftL cl++shiftR cl, not (isTrue e)]
   ilClauses cl  = [ hc | hc@(Fu fus)<-allClauses cl
                        , and [idsOnly e| t@(Cp e)<-fus]
                   ]
   irClauses cl  = [ hc | hc@(Fu fus)<-allClauses cl
                        , and [idsOnly t| t@(F fs)<-fus]
                   ]
   iClauses  cl  = [ hc | hc@(Fu fus)<-allClauses cl
                        , and [idsOnly e| t@(Cp e)<-fus] || and [idsOnly t| t@(F fs)<-fus]
                   ]
   niClauses cl  = [ hc | hc@(Fu fus)<-allClauses cl
                        , not (and [idsOnly e| t@(Cp e)<-fus] || and [idsOnly t| t@(F fs)<-fus])
                   ]
   lClause :: Expression -> Expression
   lClause cl    = head (niClauses cl++[cl])
   rClause :: Expression -> Expression
   rClause cl    = last ([cl]++niClauses cl)




   hornCs :: Rule -> Expression -> [ComputeRule]
   hornCs rule f@(Fu fus)
    = 
  -- the following inserts new atoms in positive terms. Deletion of new atoms from negative terms cannot occur,
  -- because an atom which is new in V is not in t in the first place (t-:V)
      [ CR ( [("INSERT INTO", Vs (source t) (target t))]     -- fOps
        , v (sign t)                                         -- e
        , "INSERT INTO"                                      -- bOp
        , simplify t                                           -- toExpr
        , v (sign t)                                         -- frm
        , rule)
      | and (map isPos fus), t<-fus, not (isIdent t)]++  -- (ignore generating to I, because both I and V are derived from C-tables.)
      [ CR ( [("DELETE FROM",hdl l)]              -- fOps
        , if isPos t' then t' else notCp t'       -- e
        , "DELETE FROM"                           -- bOp
        , (conjNF.Cp) t                    -- toExpr
        , (disjNF.Cp) (Fu (rest t))        -- frExpr
        , rule)
      | t<-fus, t'<-rest t, l<-leaves t', isPos l, isNeg t]++
      [ CR ( [("INSERT INTO",hdl l)]
        , if isPos t' then t' else notCp t'
        , "DELETE FROM"
        , (conjNF.Cp) t
        , (disjNF.Cp) (Fu (rest t))
        , rule)
      | t<-fus, t'<-rest t, l<-leaves t', isNeg l, isNeg t]++
      [ CR ( [("DELETE FROM",hdl l)] 
        , if isPos t' then t' else notCp t'
        , "INSERT INTO"
        , conjNF t   
        , (disjNF.Cp) (Fu (rest t))
        , rule)
      | t<-fus, t'<-rest t, l<-leaves t', isPos l, isPos t]++
      [ CR ( [("INSERT INTO",hdl l)] 
        , if isPos t' then t' else notCp t'
        , "INSERT INTO"
        , conjNF t   
        , (disjNF.Cp) (Fu (rest t))
        , rule)
      | t<-fus, t'<-rest t, l<-leaves t', isNeg l, isPos t]
      where rest t = if length [e|e<-fus, t /= e] == length fus-1 then [e|e<-fus, t /= e] else
                     error ("(module Calc) Failure in hornCs rule f@(Fu fus) with\n"++
                            "Rule : "++showADL rule++"\n"++
                            "t    : "++showADL t++"\n"++
                            "t    : "++showHS "" t++"\n"++
                            "f    : "++showADL f++"\n"++
                            "f    : "++showHS "" f++"\n"
                           )
            hdl l | null (declarations l) = error("Module Calc: empty list of declarations in hornCs")
                  | otherwise             = head (declarations l)
   hornCs rule e = error("Module Calc: erroneous call of hornCs ("++showHS "" e++") in rule "++show (nr rule)++":\n  "++showADL rule)



   leaves e = rd (lvs e)
    where
     lvs (F fs)  = (concat.map lvs) fs
     lvs (Fd fs) = (concat.map lvs) fs
     lvs (Fu fs) = (concat.map lvs) fs
     lvs (Fi fs) = (concat.map lvs) fs
     lvs (Cp e)  = [notCp l|l<-lvs e ]
     lvs (Tm r)  = [Tm r]
     lvs (K0 e)  = lvs e
     lvs (K1 e)  = lvs e
     lvs e = error("module Calc: illegal pattern in leaves ("++showADL e++")\ne = "++showHS "" e)

   data ComputeRule = CR ([(String,Declaration)],Expression,String,Expression,Expression,Rule) deriving (Eq)
   instance Show ComputeRule where
    showsPrec p (CR (fOps, e, bOp, toExpr, frExpr, rule))
     = showString ("("++show fOps++", "++show e++", "++show bOp++", "++show toExpr++", "++show frExpr++", "++show rule++")")

   triggers :: Rule -> [ComputeRule]
   triggers rule
    = (concat.map (sort' bop).eqClass eq2expr)          --  < ---  bij gelijke targets: eerst DELETE dan INSERT
      [ hc ::ComputeRule
      | conjunct<-conjuncts rule, clause<-allClauses conjunct
      , hcID<-(map collect.eqClass eqHC.hornCs rule) clause  --  < ---  alle gelijke horn clauses op hoopjes vegen.
      , hc<-splitInsDel hcID
      , computing rule hc]
      where
   -- eerst alle gelijke horn clauses op hoopjes vegen.
       ( CR (fOps, e, bOp, toExpr, frExpr, rule)) `eqHC` (CR (fOps', e', bOp', toExpr', frExpr', rule'))
              =      (bOp, toExpr, frExpr)          ==                  (bOp', toExpr', frExpr')
       collect :: [ComputeRule] -> ComputeRule
       collect cl = CR ((rd.concat)[fOps| CR (fOps, e, bOp, toExpr, frExpr, rule) <-cl], simplify (Fu [e| CR (fOps, e, bOp, toExpr, frExpr, rule)<-cl]), bOp, toExpr, frExpr, rule)
        where CR (fOps, e, bOp, toExpr, frExpr, rule) = head cl
       splitInsDel :: ComputeRule -> [ComputeRule]
       splitInsDel (CR (fOps, e, bOp, toExpr, frExpr, rule))
  --      = [ ([(f,r)|(f, CR (r, e, bOp, toExpr, frExpr, rule))<-cl], e, bOp, toExpr, if f=="DELETE FROM" then (notCp frExpr) else frExpr, rule)
  --        | cl<-eqCl fst [(f, CR (r, e, bOp, toExpr, frExpr, rule))| (f,r)<-fOps]
  --        ]
        = [ CR ([fOp], e, bOp, toExpr, frExpr, rule)| fOp@("DELETE FROM",r)<-fOps]++
          [ CR ([fOp], e, bOp, toExpr, frExpr, rule)| fOp@("INSERT INTO",r)<-fOps]
   -- volgorde aanbrengen in hornclauses met gelijke toExpr: eerst DELETE dan INSERT
       CR (fOps, e, bOp, toExpr, frExpr, r) `eq2expr` CR (fOps', e', bOp', toExpr', frExpr', r') = toExpr == toExpr'
       bop (CR (fOps, e, bOp, toExpr, frExpr, r)) = bOp
   -- alleen "COMPUTING" termen opleveren
       computing rule hc@(CR (fOps, e, bOp, toExpr, frExpr, r)) = toExpr `elem` map simplify (r_cpu rule)
   -- debug:    computing rule e = error ("(module Calc diagnostic) rule: "++showADL rule++"\e: "++show e++"\nr_cpu rule : "++ show (e `elem` r_cpu rule))

   multDerivations :: Language pat => pat -> Declarations
   multDerivations context
    = rd ([d| d<-declarations context, not (null ([Tot,Sur] `isc` multiplicities d))]++
          [ d
          | rule<-declaredRules context, conjunct<-conjuncts rule, clause<-ilClauses conjunct
          , Fi fs<-[conjNF clause], Fu fus<-fs
          , if and [isIdent c| c<-fus, isNeg c] then True else error (" in module Calc, multDerivations "++showADL clause)
          , F ts<-fus -- t<-fus, isPos t
          , if not (null ts) then True else error (" in module Calc, multDerivations: null ts ")
          , d<-(map (add Tot) . declarations . head) ts++(map (add Sur) . declarations . last) ts
          ])
      where add m (Sgn nm a b props prL prM prR cs expla pos nr sig) = (Sgn nm a b (rd (m:props)) prL prM prR cs expla pos nr sig)

   instance Graphic Declarations where
    dotGraph context style nm links
     = "digraph "++show [x|x<-nm,not(isSpace x)]++introG++newline
             ++ chain newline
                ([ line (name (source d)) (name d) (name (target d)) | d<-links, Tot `elem` multiplicities d]++
                 [ line (name (target d)) (name d) (name (source d)) | d<-links, Sur `elem` multiplicities d])
             ++ "\n   }"
       where
         introG = "\n   { bgcolor=transparent"++newline
               ++ " { node [shape=box,fontsize=18,font=helvetica] "++(chain "; ".map quote.sord) [name c| d<-links, c<-[source d, target d]]++" }"++newline
               ++ "node [shape=plaintext,fontsize=18,font=helvetica]"++newline
               ++ "edge [fontsize=12,arrowsize=0.8,len=2]"
         line p1 s p2 = quote (p1) ++ edgearrow ++ quote (p2) ++ if null s then "" else " [label="++quote s++"]"
         --- Nog wat hulpfuncties. Die horen overigens waarschijnlijk niet hier...
         edgearrow = " -> "
         newline = "\n   ; "
         quote s = "\"" ++ s ++ "\" "

   instance Graphic [ECArule] where
    dotGraph context style nm links
     = "digraph "++show [x|x<-nm,not(isSpace x)]++introG++newline
             ++ chain newline
                [ line f "" t | l@(ECA (On fOp frm) (Do tOp tExpr delta))<-lnks
                              , (f,t)<-[(showADL frm,show l),(show l,showADL tExpr)]]
             ++ "\n   }"
       where
         introG = "\n   { bgcolor=transparent"++newline  -- the overlap=False takes time!   ++"overlap=False"++newline
               ++ " { node [shape=ellipse,fontsize=18,font=helvetica] "++(chain "; ".map quote.sord) [c| ECA (On fOp frm) (Do tOp tExpr delta)<-lnks, c<-[showADL frm, showADL tExpr]]++" }"++newline
               ++ " { node [shape=box,fontsize=12,font=helvetica] "++(chain "; ".sord) ["{"++quote (show l)++" ["++lbl l++"]} "| l<-lnks]++" }"++newline
               ++ "node [shape=plaintext,fontsize=18,font=helvetica]"++newline
               ++ "edge [fontsize=12,arrowsize=1.2,len=2]"
         line p1 s p2 = quote (p1) ++ edgearrow ++ quote (p2) ++ if null s then "" else " [label="++quote s++"]"
         lbl (ECA (On fOp frm) (Do tOp tExpr delta))
          = "label="++quote("ON "++show fOp++" DO "++show tOp++" "++showADL delta)
         lnks = [ ECA (On fOp (if inline frm then frm else flp frm))
                      (Do tOp (if p tExpr then tExpr else flp tExpr) delta)
                | ECA (On fOp frm) (Do tOp tExpr delta)<-links ]
         p (Tm m)  = inline m
         p (F [])  = True
         p (F ts)  = p (head ts)
         p (Fd []) = True
         p (Fd ts) = p (head ts)
         p (Fi []) = True
         p (Fi fs) = p (head fs)
         p (Fu []) = True
         p (Fu fs) = p (head fs)
         p (Cp e)  = p e
         p (K0 e)  = p e
         p (K1 e)  = p e
         p e       = True

         --- Nog wat hulpfuncties. Die horen overigens waarschijnlijk niet hier...
         edgearrow = " -> "
         newline = "\n   ; "
         quote s = "\"" ++ s ++ "\" "

   closRule [] = error ("Module Calc: empty argument in closRule.")
   closRule xs
     = if or [null x| x<-xs] then error ("Module Calc: empty list in closRule "++show xs) else
       f xs (rd (map head xs) `isc` rd (map last xs))
       where
        f q (x:xs) = f (q `uni` [[head ls,last rs]| ls<-q,last ls==x ,rs<-q,head rs==x]) xs
        f q []     = q
















   computeOrder :: [ComputeRule] -> String -> [Declaration] ->[ComputeRule]
   computeOrder hcs         -- hcs is the entire set of compute rules
                onOperation -- "INSERT INTO", "DELETE FROM", or "UPDATE".
                ss          -- the set of declarations at the beginning of the compute-chain.
    = if True then paths else -- False maken voor diagnose...
      error ( "(diagnostic in module Calc)\ncomputeOrder hcs ["++chain "," (map showADL ss)++"] = \n"++
               "(hcs: \n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc++"\n  "++chain "\n  " (map showADL (declarations frExpr))
                                          | hc@(CR (fOps, e, bOp, toExpr, frExpr, rule))<-hcs]++"\n\nsel: "++
               "\n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc| hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-sel]++"\n)\n\npaths:"++
               "\n  "++chain "\n  " [informalRule {-(declarations frExpr)-} hc| hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-paths] )
      where
       (CR (fOps, e, bOp, toExpr, frExpr, rule)) `before` (CR (fOps', e', bOp', toExpr', frExpr', rule'))
        -- = not (null (declarations toExpr `isc` declarations frExpr')) && bOp==fOp'
        = (not.null) [(fOp',r) |(fOp',r)<-fOps', bOp==fOp', r `elem` declarations toExpr]
       sel
        = combineTriggers
            ([ CR (fOps', e, bOp, toExpr, frExpr, rule)
             | hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-hcs                        -- men neme een kandidaatregel, hc
             , isTrue frExpr                                                       -- waarvan de antecedent persé waar is
             , fOps'<-[[on|on@(fOp,s)<-fOps, elem' typeEq s ss]], not (null fOps') -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst fOps else True
             ]++
             [ if True {- debug: nr rule/=12 -} then (CR (fOps', e, bOp, toExpr, frExpr, rule)) else
               error ( "(diagnostic in module Calc)" ++
                       "\nfrExpr (showADL)    = "++showADL frExpr ++
                       "\nfrExpr (showHS)     = "++showHS "" frExpr ++
                       "\ndeclarations frExpr = "++showHS "" (declarations frExpr) ++
                       "\nss                  = "++showHS "" ss  ++
                       "\nnot (null (ss `isc` declarations frExpr)) = "++show(not (null (ss `isc` declarations frExpr)))
                     )
             | hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-hcs                        -- men neme een kandidaatregel, hc
             , not (isTrue frExpr)                                                 -- die een antecedent heeft die niet persé waar is
             , fOps'<-[[on|on@(fOp,s)<-fOps, s `elem ` ss]], not (null fOps') -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst fOps else True
             , not (null (ss `isc` declarations frExpr)) ]++
             [ CR (fOps', e, bOp, toExpr, frExpr, rule)
             | hc@ (CR (fOps, e, bOp, toExpr, frExpr, rule))<-hcs                        -- men neme een kandidaatregel, hc
             , isIdent frExpr                                                      -- met de identiteit als antecedent
             , fOps'<-[[on|on@(fOp,s)<-fOps, s `elem ` ss]], not (null fOps') -- en die wordt aangetrapt
             , if onOperation `elem` ["INSERT INTO", "DELETE FROM"] then onOperation `elem` map fst fOps else True
             ])
       typeEq a b | isIdent a && isIdent b = source a `order` source b
                  | otherwise              = a==b
    -- define tuples, which are paths of length 1
       rrs = [[x,x']| x<-clossel, x'<-clossel, x `before` x']
    -- taking the transitive closure means to compute paths of arbitrary length. rs is such a path
       paths    = sort isin clossel
       isin a b = if null rrs then False else [a,b] `elem` closRule rrs
       sort f [] = []
       sort f (x:xs) = sort f [e|e<-xs, f e x] ++ [x] ++ sort f [e|e<-xs, not (f e x)]
       identifies (CR (fOps, e, bOp, toExpr, frExpr, r)) = (bOp, toExpr, frExpr)
       clossel = f [] sel [hc| hc<-hcs, not (identifies hc `elem` map identifies sel)]
        where f sel [] rs = sel
              f sel new rs = f (sel++new) [r|r<-rs, or [n `before` r| n<-new], not (r `elem` new)] [r|r<-rs, and [not (n `before` r)| n<-new]]

   combineTriggers :: [ComputeRule] -> [ComputeRule]
   combineTriggers hcs -- Triggers die toevallig semantisch identiek zijn worden op één hoop geveegd.
    = [ if length cl==1 then head cl else
        CR( rd [fOp|(CR (fOps, e, bOp, toExpr, frExpr, rule))<-cl, fOp<-fOps]
          , e, bOp, toExpr, frExpr
          , Ru Implication frExpr posNone toExpr [] 
              (chain "; " (rd[explain rule++" ("++show (pos rule)++")"|(CR (fOps, e, bOp, toExpr, frExpr, rule))<-cl, (not.null.explain) rule]))
              (sign frExpr `lub` sign toExpr) 0 ""
        )
      |cl<-eqCl f hcs, CR (_,e,bOp,toExpr,frExpr,_)<-[head cl]  ]
      where
       f (CR (fOps, e, bOp, toExpr, frExpr, rule)) = (bOp, toExpr, frExpr)

   informalRule :: ComputeRule -> String
   informalRule (CR (fOps, e, bOp, toExpr, frExpr, rule))
    = "ON "++commaEng "OR" [fOp++" "++if isSgn r then name r else showADL r|(fOp,r)<-fOps] ++" DO "++bOp++" "++showADL toExpr++" SELECTFROM "++sh frExpr
      where sh x = if isTrue x then "V["++(chain ",".rd.map name) [source x,target x]++"]" else showADL x

   instance Show ECArule where
    showsPrec p (ECA event pa) = showString (show event++"\nEXECUTE "++show pa)
   instance Show Event where
    showsPrec p (On Ins m) = showString ("ON INSERT Delta IN "++show m)
    showsPrec p (On Del m) = showString ("ON DELETE Delta FROM "++show m)

   instance Show PAclause where
    showsPrec p fragm = showString (showFragm "\n        " fragm)
     where
      showFragm indent (Do Ins tExpr delt) = "INSERT INTO "++sh tExpr++" SELECTFROM "++sh delt
      showFragm indent (Do Del tExpr delt) = "DELETE FROM "++sh tExpr++" SELECTFROM "++sh delt
      showFragm indent (Choice ds)
        = "ONE of "++chain (indent++"       ") [showFragm (indent++"       ") d | d<-ds]
      showFragm indent (All ds)
        = "ALL of "++chain (indent++"       ") [showFragm (indent++"       ") d | d<-ds]
      sh x = if isTrue x then "V["++(chain ",".rd.map name) [source x,target x]++"]" else showADL x


   recalc :: Context -> Context
   recalc context = update (foldr subst (declarations context) calcrules) context
    where
     calcrules = computeOrder [hc| rule<-rules context, hc@ (CR (fOps, e, "INSERT INTO", toExpr, frExpr, rule))<-triggers rule, "INSERT INTO" `elem` [fOp|(fOp,r)<-fOps] ] "UPDATE" (declarations context)
     subst hc@(CR (fOp, e, bOp, toExpr, frExpr, rule)) ss
      = [ if [s]==declarations toExpr then insert (calc frExpr ss) s else s
        | s<-ss ]
     insert :: Pairs -> Declaration -> Declaration
     insert ls (Sgn nm a b props prL prM prR cs expla pos nr sig) = Sgn nm a b props prL prM prR (ls `uni` cs) expla pos nr sig
     insert ls id                                                 = id




   norm1Rule :: Rule -> Expression
   norm1Rule = conjNF.normExpr
   makeRule :: Rule -> Expression -> Rule
   makeRule r (Fu []) = error ("(module Calc:) erroneous call to function makeRule r ("++showADL (Fu [])++").")
   makeRule r (Fu ts)
    | or [isNeg t|t<-ts] = Ru Implication (Fi [notCp t|t<-ts,isNeg t]) (rrfps r) (Fu [t|t<-ts,isPos t]) [] (rrxpl r) (rrtyp r) (runum r) (r_pat r)
    | otherwise          = Ru AlwaysExpr (error ("(Module Calc: ) erroneous call to antecedent of r "++showADL (Fu ts))) (rrfps r) (Fu ts) [] (rrxpl r) (rrtyp r) (runum r) (r_pat r)
   makeRule r e
    | isFu e'   = makeRule r e'
    | otherwise = Ru AlwaysExpr (error ("(Module Calc: ) erroneous call to antecedent of r "++showADL e)) (rrfps r) e [] (rrxpl r) (rrtyp r) (runum r) (r_pat r)
    where e' = disjNF e







   type Proof = [(Expression,[String],String)]
   reversePrf [] = []
   reversePrf [s] = [s]
   reversePrf ((r,cs,e):prf@((r',cs',e'):prf')) = init rp++[(r',cs,rev e),(r,[],"")]
     where rp = reversePrf prf
           rev "==>" = "<=="
           rev "<==" = "==>"
           rev "-->" = "<--"
           rev "<--" = "-->"
           rev x = x

   showProof [(e,ss,equ)]     = "\n      "++showADL e++"\n"
   showProof ((e,ss,equ):prf) = "\n      "++showADL e++
                                "\n"++(if null ss then "\n   "++equ else if null equ then chain " " ss else "   "++equ++" { "++chain "; " ss++" }")++
                                showProof prf
                                where e'= if null prf then "" else let (e,ss,equ):_ = prf in showHS "" e 
   showProof []               = ""






   nfProof expr = nfPr True False expr -- Clauses are derived by means of the conjunctive form, using <=> derivations.
   nfPr eq dnf expr
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):nfPr eq dnf (simplify res)
    where (res,steps,equ) = normStep eq dnf False expr



   conjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr True  False expr
   conjNF' expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr False False expr
   disjNF  expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr True  True expr
   disjNF' expr = negRight (if null proof then expr else expr')
                  where (expr',motives,equ) = last proof
                        proof = nfPr False True expr
   negRight e = e









   derivMono expr tOp m = f (head (lambda tOp (Tm m) expr++[[]])) (start tOp)
    where
     f [] (neg,pos) = []
     f [(e,c,d)] (neg,pos)
      = [(rule (subst (Tm m,neg) e) (subst (Tm m,pos) e),[],"")]
     f ((e,["omkeren"],deduce): prf@((e',c',d'):ps)) (neg,pos)
      = (rule (subst (Tm m,neg) e) (subst (Tm m,pos) e),["r -: s  <=>  s- -: r-"],"<=>"):
         f prf (pos,neg)
     f ((e,comment,deduce): prf@((e',c',d'):ps)) (neg,pos)
      = (rule (subst (Tm m,neg) e) (subst (Tm m,pos) e),["Monotony of "++showOp e'],"==>"):
         f prf (neg,pos)
     start Ins  = (Tm m,Fu [Tm m,delta (sign m)])
     start Del  = (Fi [Tm m,Cp (delta (sign m))],Tm m)
     rule neg pos | isTrue neg = Ru AlwaysExpr (error ("(Module Calc:) illegal reference to antecedent in rule ("++showADL neg++") ("++showADL pos++")")) posNone pos [] "" (sign neg {- (neg `lub` pos) -}) 0 ""
                  | otherwise  = Ru Implication neg posNone pos [] "" (sign neg {- (neg `lub` pos) -}) 0 ""
     showOp (F fs) = ";"
     showOp (Fd fs) = "!"
     showOp (Fu fs) = "\\/"
     showOp (Fi fs) = "/\\"
     showOp (Cp e) = "-"
     showOp (K0 e) = "*"
     showOp (K1 e) = "+"
     showOp (Tm m) = if inline m then "" else "~"






































   lambda :: InsDel -> Expression -> Expression -> [Proof]
   lambda tOp e expr = [reversePrf[(e,text,op)| (e,func,text,op)<-prf]| prf<-lam tOp e expr ]
    where
       lam tOp e (F [f])   = lam tOp e f
       lam tOp e f@(F fs)  | e==f                = [[(e,(\x->x),[],"")]]
                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)] -- isNeg is nog niet helemaal correct.
                           | or[null p|p<-fPrfs] = []
                           | otherwise           = [(f,(\x->f),[derivtext tOp "mono" (first lc) f],"<--"): lc]
                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]



                                   vars  = map head fPrfs -- wordt niet aangeroepen als er een lege afleiding in fPrfs zit
                                   deMrg (F fs) = notCp (Fd [notCp f| f<-fs])
                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
                                   rc    = remainders vars vars







       lam tOp e  (Fu [f]) = lam tOp e f
       lam tOp e f@(Fu fs) | e==f                = [[(e,(\x->x),[],"")]]
                           | length const>0      = [(f,(\x->f),      [derivtext tOp "mono" inter f],"<--") :prf     | prf<-lam tOp e inter]
                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)]
                           | or[null p|p<-fPrfs] = []
                           | otherwise           = [(f,(\x->f),      [derivtext tOp "mono" (first lc) f],"<--") : lc]
                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
                                   vars  = map head fPrfs
                                   const = [f|f<-fs, isConst f e]
                                   inter = Fu [f|f<-fs, isVar f e]
                                   deMrg (Fu fs) = notCp (Fi [notCp f| f<-fs])
                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
                                   rc    = remainders vars vars
       lam tOp e (Fd[f])   = lam tOp e f
       lam tOp e f@(Fd fs) | e==f                = [[(e,(\x->x),[],"")]]
                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)] -- isNeg is nog niet helemaal correct.
                           | or[null p|p<-fPrfs] = []
                           | otherwise           = [(f,(\x->f),[derivtext tOp "mono" (first lc) f],"<--"): lc]
                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]



                                   vars  = map head fPrfs -- wordt niet aangeroepen als er een lege afleiding in fPrfs zit
                                   deMrg (Fd fs) = notCp (F [notCp f| f<-fs])
                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
                                   rc    = remainders vars vars
       lam tOp e  (Fi [f]) = lam tOp e f
       lam tOp e f@(Fi fs) | e==f                = [[(e,(\x->x),[],"")]]
                           | length const>0      = [(f,(\x->f),      [derivtext tOp "mono" inter f],"<--") :prf     | prf<-lam tOp e inter]
                           | and [isNeg f|f<-fs] = [(f,(\x->deMrg x),[derivtext tOp "gelijk" (deMrg f) f],"==") :prf| prf<-lam tOp e (deMrg f)]
                           | or[null p|p<-fPrfs] = []
                           | otherwise           = [(f,(\x->f),      [derivtext tOp "mono" (first lc) f],"<--") : lc]
                             where fPrfs = [lam tOp e f|f<-fs, isVar f e]
                                   vars  = map head fPrfs
                                   const = [f|f<-fs, isConst f e]
                                   inter = Fi [f|f<-fs, isVar f e]
                                   deMrg (Fi fs) = notCp (Fu [notCp f| f<-fs])
                                   lc    = longstcomn vars++concat (drop (length rc-1) (sort' length rc))
                                   rc    = remainders vars vars
       lam tOp e f@(K0 x)  = [(f,(\x->K0 x),[derivtext tOp "mono" x f],"<--") :prf   | prf<-lam tOp e x]
       lam tOp e f@(K1 x)  = [(f,(\x->K1 x),[derivtext tOp "mono" x f],"<--") :prf   | prf<-lam tOp e x]
       lam tOp e f@(Cp x)  = [(f,(\x->Cp x),["omkeren"],"<--") :prf| prf<-lam (inv tOp) e x]
       lam tOp e   (Tc x)  = lam tOp e x





       lam tOp e f@(Tm m)  = [[(e,(\x->x),[],"")]]

 --      lam tOp e f       = []
  -- longstcomn determines the longest prefix common to all xs in xss.
       longstcomn xss | or [null xs| xs<-xss]      = []
                      | length (eqCl first xss)==1 = head [head prf| prf<-xss]: longstcomn [tail prf| prf<-xss]
                      | otherwise                  = []
  -- remainders determines the remainders.
       remainders hs xss | or [null xs| xs<-xss]      = xss
                         | length (eqCl first xss)==1 = remainders xss [tail prf| prf<-xss]
                         | otherwise                  = xss
       isConst e f = null (declarations e `isc` declarations f)
       isVar e f   = not (isConst e f)
       derivtext tOp "omkeren" e expr = sh tOp++showADL e++" means "++sh (inv tOp)++showADL expr++"."
       derivtext tOp "mono" e expr = "("++showADL e++"->"++showADL expr++") is monotonous, so "++sh tOp++showADL e++" means "++sh tOp++showADL expr++"."
        where x = take 1 [c| c<-['x'..'z']++['a'..'w'], not ([c] `elem` [name m| m<-mors expr])]
       derivtext tOp str e expr = str
       derivfunc e expr = (\x-> subst (e,x) expr)
       sh Ins  = "insert into "
       sh Del  = "delete from "
       inv Ins = Del
       inv Del = Ins
       first ((e,morphism,text,str):rest) = e
       first _ = error "Module Calc: wrong pattern in first"

   simplify :: Expression -> Expression
   simplify expr = e where (e,_,_) = last (simpProof expr)
   
   simpProof expr    -- dus levert op: [(Expression,[String],String)]
    = if expr==res
      then [(expr,[],"<=>")]
      else (expr,steps,equ):simpProof res
    where (res,steps,equ) = normStep True True True expr



   normStep :: Bool -> Bool -> Bool -> Expression -> (Expression,[String],String)
   normStep eq    -- If eq==True, only equivalences are used. Otherwise, implications are used as well.
            dnf   -- If dnf=True, the result is in disjunctive normal form. Otherwise it is in conjunctive normal form
            simpl -- If True, only simplification rules are used, which is a subset of all rules. Consequently, simplification is implied by normalization.
            expr = (res,ss,equ)
    where
     (res,ss,equ) = norm expr []
     norm :: Expression -> [Expression] -> (Expression,[String],String)
     norm (K0 e)       rs   = (K0 res, steps, equ)
                              where (res,steps,equ) = norm e []
     norm (K1 e)       rs   = (K1 res, steps, equ)
                              where (res,steps,equ) = norm e []
     norm (Cp (Cp e))  rs   = (e, ["compl of compl"],"<=>")
     norm (Cp (Fi fs)) rs   = if simpl then (notCp res,steps,equ) else (Fu (map notCp fs), ["De Morgan"], "<=>")
                              where (res,steps,equ) = norm (Fi fs) []
     norm (Cp (Fu fs)) rs   = if simpl then (notCp res,steps,equ) else (Fi (map notCp fs), ["De Morgan"], "<=>")
                              where (res,steps,equ) = norm (Fu fs) []
     norm (Cp (Fd ts)) rs   = if not simpl && and [isNeg t| t<-ts] then (F (map notCp ts), ["De Morgan"], "<=>") else
                              (notCp res,steps,equ)
                              where (res,steps,equ) = norm (Fd ts) []
     norm (Cp e)       rs   = (notCp res,steps,equ)
                              where (res,steps,equ) = norm e []
     norm (Tc f)       rs   = norm f []
     norm (F [t])      rs   = norm t []
     norm (F (e:es))   rs   | or [isF x|x<-e:es]          = norm (F [y| x<-e:es, y<-if isF x then unF x else [x]]) rs  -- haakjes verwijderen o.g.v. associativiteit
                            | or [isIdent x|x<-e:es]      = (F [x|x<-e:es,not (isIdent x)], ["x;I = x"], "<=>")
                            | not simpl && not eq && length es>1 && isFd g && length gs>1
                                                          = (F (Fd (F [e,head gs]:tail gs):es'), ["Peirce: r;(s!q) => (r;s)!q"],"==>")
                            | not simpl && not eq && isFd e && length ue>1
                                                          = (Fd (init ue++[F (last ue:es)]), ["Peirce: (r!s);q => r!(s;q)"],"==>")
                            | not simpl && not eq && isFi e = (distribute F Fi isF isFi (F (e:es)), ["distribute /\\ over ;"], "==>")
                            | not simpl && isFu e           = (distribute F Fu isF isFu (F (e:es)), ["distribute \\/ over ;"], "<=>")
                            | not simpl && and [isNeg x|x<-(e:es)]
                                                          = (notCp (Fd [notCp x| x<-(e:es)]), ["De Morgan"], "<=>")
                            | not simpl && isFu (last (e:es)) = (Fu [F (init (e:es)++[t])|Fu xs<-[last (e:es)], t<-xs], ["distribute \\/ over ;"], "<=>")
                            | otherwise                   = (if isF f then F (t:unF f) else F [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (F es) (e:rs)
                                  ue = unF e
                                  g@(Fd gs):es' = es
     norm (Fd [e])    rs    = norm e []
     norm (Fd (e:es)) rs    | or [isFd x|x<-e:es]         = norm (Fd [y| x<-e:es, y<-if isFd x then unF x else [x]]) rs
                            | or [isNot x|x<-e:es]        = (F [x|x<-e:es,not (isNot x)], ["x!-I = x"], "<=>")
                            | not simpl && not eq && isFu e = (distribute Fd Fu isFd isFu (Fd (e:es)), ["distribute \\/ over !"], "==>")
                            | not simpl && isFi e         = (distribute Fd Fi isFd isFi (Fd (e:es)), ["distribute /\\ over !"], "<=>")
                            | not simpl && and [isNeg x|x<-(e:es)]
                                                          = (notCp (F [notCp x| x<-(e:es)]), ["De Morgan"], "<=>")
                            | not simpl && length es>1 && isNeg e && isPos g && isFunction e
                                                          = (F [notCp e,Fd es], ["f-!g = f;g if f is a function"], "<=>")
                            | not simpl && length es>1 && isPos e && isNeg g && isFunction (flp g)
                                                          = (Fd ( F[e,notCp g]:es'), ["f!g- = f;g if g~ is a function"], "<=>")
                            | otherwise                   = (if isFd f then Fd (t:unF f) else Fd [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fd es) (e:rs)
                                  ue = unF e
                                  g:es' = es
     norm (Fi [e]) rs       = norm e []
     norm (Fi (e:es)) rs    | or [isFi x|x<-e:es]         = norm (Fi [y| x<-e:es, y<-if isFi x then unF x else [x]]) rs
                            | rd(e:es)/=e:es              = (Fi (rd (e:es)), ["x/\\x = x"], "<=>")
                            | not (null incons)           = (Fu [], [showADL (notCp (head incons))++"/\\"++showADL (head incons)++" = V-"], "<=>")
                            | e==Fu []                    = (Fu [], ["inconsistency"], "<=>")
  -- this is unreachable    | e==Fi []                    = (Fi es, ["x/\\V = x"], "<=>")
                            | or[x==Fu []|x<-es]          = (Fu [], ["x/\\V- = V-"], "<=>")
                            | isFu e && not (null absor0) = let f=head absor0 in (Fi es, ["absorb "++showADL e++" because of "++showADL f], "<=>")
                            | isFu e && not (null absor1) = let (ts,f)=head absor1 in (Fi (ts++es), ["absorb "++showADL f], "<=>")
                            | not simpl && isFu e && dnf  = (distribute Fi Fu isFi isFu (Fi (e:es)), ["distribute \\/ over /\\"], "<=>")
                            | otherwise                   = (if isFi f then Fi (t:unF f) else Fi [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fi es) (e:rs)
                                  incons = [x|x<-es,x==notCp e]
                                  absor0 = [t| t<-unF e, f<-es++rs, t==f]
                                  absor1 = [(if length rest<=1 then rest else [Fu rest] , t)| t<-unF e, f<-es++rs, notCp t==f, rest<-[[x|x<-unF e,x/=t]]]
     norm (Fu [e]) rs       = norm e []
     norm (Fu (e:es)) rs    | or [isFu x|x<-e:es]         = norm (Fu [y| x<-e:es, y<-if isFu x then unF x else [x]]) rs
                            | rd(e:es)/=e:es              = (Fu (rd (e:es)), ["x\\/x = x"], "<=>")
                            | not (null compl)            = (Fi [], [showADL (notCp (head compl))++"\\/"++showADL (head compl)++" = V"], "<=>")
                            | e==Fi []                    = (Fi [], ["tautology"], "<=>")
  -- this is unreachable    | e==Fu []                    = (Fu es, ["x\\/V- = x"], "<=>")
                            | or[x==Fi []|x<-es]          = (Fi [], ["x\\/V = V"], "<=>")
                            | isFi e && not (null absor0) = let f=head absor0 in (Fu es, ["absorb "++showADL e++" because of "++showADL f++" ((x/\\y)\\/y = y))"], "<=>")
                            | isFi e && not (null absor1) = let (ts,f)=head absor1 in (Fu (ts++es), ["absorb "++showADL f++" ((x/\\y-)\\/y = x\\/y))"], "<=>")
                            | not simpl && isFi e && not dnf = (distribute Fu Fi isFu isFi (Fu (e:es)), ["distribute /\\ over \\/"], "<=>")
                            | otherwise                   = (if isFu f then Fu (t:unF f) else Fu [t,f], steps++steps', fEqu [equ,equ'])
                            where (t,steps, equ)  = norm e []
                                  (f,steps',equ') = norm (Fu es) (e:rs)
                                  compl  = [x|x<-es,x==notCp e]
                                  absor0 = [t| t<-unF e, f<-es++rs, t==f]
                                  absor1 = [(if length rest<=1 then rest else [Fi rest] , t)| t<-unF e, f<-es++rs, notCp t==f, rest<-[[x|x<-unF e,x/=t]]]
     norm x           rs    = (x,[],"<=>")





   fEqu ss = if and [s=="<=>" | s<-ss] then "<=>" else "==>"
   isFu (Fu fs) = True
   isFu _       = False
   isFi (Fi fs) = True
   isFi _       = False
   isF  (F _)   = True
   isF _        = False
   isFd (Fd _)  = True
   isFd _       = False
   unF (Fi es)  = es
   unF (Fu es)  = es
   unF (Fd es)  = es
   unF (F  es)  = es
   unF x        = [x]








   distribute f g isf isg = dis
    where
     dis x | isf x && null xs = g [f []]
           | isg x && null xs = g []
           | isg x && isg e   = dis (g (ys++es))
           | isf x && isf e   = dis (f (ys++es))
           | isf x            = g [f [p,q]| p<-if isg e then unF e else [e], q<-unF (dis (f es))]
           | isg x            = g (unF (dis e)++es)
  --       | null es          = g [e]
           | otherwise        = g [x]             
           where xs = unF x
                 e:es = xs
                 ys = unF e

   normR r@(Ru AlwaysExpr antc pos cons cpu expla sgn nr pn) = Ru AlwaysExpr err pos (conjNF cons) (r_cpu r) expla sgn nr (r_pat r)
    where err = error ("Module Calc: erroneous reference to antc of rule "++showADL r)
   normR r@(Ru c antc pos cons cpu expla sgn nr pn)   = Ru c (disjNF antc) pos (conjNF cons) (r_cpu r) expla sgn nr (r_pat r)
   normR r@(Sg p rule expla sgn nr pn signal)         = Sg p (normR rule) expla sgn nr (r_pat r) signal
   normR r@(Gc pos m expr cpu sgn nr pn)              = error "Calc.lhs: normR op glue regel. Gc pos m (conjNF expr) (r_cpu r) sgn nr (r_pat r)"



















   unVee (Fi es) | or [ x==Cp y | x<-es, y<-es ] = Cp (v (sign (Fi es)))
                 | or [ isFalse x | x<-es ]      = Cp (v (sign (Fi es)))
                 | otherwise                     = Fi [e| e<-es, not (isTrue e)]
   unVee (Fu es) | or [ x==Cp y | x<-es, y<-es ] = v (sign (Fi es))
                 | or [ isTrue x | x<-es ]       = v (sign (Fi es))
                 | otherwise                     = Fu [e| e<-es, not (isFalse e)]
   unVee x = x

   homogeneous s = source s == target s





   shiftL :: Expression -> [Expression]
   shiftL r
    | length antss+length conss /= length fus = error ("(module Calc) shiftL will not handle argument of the form "++showHS "" r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftL doesn't work here.
    | idsOnly antss                           = [Fu ([Cp (F [Tm (mIs srcA)])]++map F conss)]
    | otherwise                               = [Fu ([ Cp (F (if null ts then id css else ts))
                                                     | ts<-ass++if null ass then [id css] else []]++
                                                     [ F (if null ts then id ass else ts)
                                                     | ts<-css++if null css then [id ass] else []])
                                                | (ass,css)<-rd(move antss conss)
                                                , if null css then error "Null css" else True
                                                , if null ass then error "Null ass" else True
                                                ]
    where
     Fu fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cp (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = -- if null antss  then error ("(module Calc) empty antecedent in shiftL ("++showHS "" r++")") else
            if length (eqClass order [ source (head ants) | ants<-antss])>1 then error ("(module Calc) shiftL ("++showHS "" r++")\n"++showADL r++"\nin calculation of srcA\n"++show (eqClass order [ source (head ants) | ants<-antss])) else
            foldr1 lub [ source (head ants) | ants<-antss]
     id ass = [Tm (mIs c)]
      where a = (source.head.head) ass
            c = if not (a `order` b) then error ("(module Calc) shiftL ("++showHS "" r++")\n"++showADL r++"\nass: "++show ass++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) ass
   -- It is imperative that both ass and css are not empty.
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move ass [] = [(ass,[])]
     move ass css
      = (ass,css):
        if and ([not (idsOnly (F cs))| cs<-css]) -- idsOnly (F [])=True, so:  and [not (null cs)| cs<-css]
        then [ts| length (eqClass (==) (map head css)) == 1
                , fun (multiplicities h)
                , ts<-move [[flp h]++as|as<-ass] (map tail css)]++
             [ts| length (eqClass (==) (map last css)) == 1
                , inj (multiplicities l)
                , ts<-move [as++[flp l]|as<-ass] (map init css)]
        else []
        where h=head (map head css); l=head (map last css)

   shiftR :: Expression -> [Expression]
   shiftR r
    | length antss+length conss /= length fus = error ("(module Calc) shiftR will not handle argument of the form "++showHS "" r)
    | null antss || null conss                = [disjuncts r|not (null fs)] --  shiftR doesn't work here.
    | idsOnly conss                           = [Fu ([Cp (F [Tm (mIs srcA)])]++map F antss)]
    | otherwise                               = [Fu ([ Cp (F (if null ts then id css else ts))
                                                     | ts<-ass++if null ass then [id css] else []]++
                                                     [ F (if null ts then id ass else ts)
                                                     | ts<-css++if null css then [id ass] else []])
                                                | (ass,css)<-rd(move antss conss)]
    where
     Fu fs = disjuncts r
     fus = filter (not.isIdent) fs
     antss = [ts | Cp (F ts)<-fus]
     conss = [ts | F ts<-fus]
     srcA = if null conss then error ("(module Calc) empty consequent in shiftR ("++showHS "" r++")") else
            if length (eqClass order [ source (head cons) | cons<-conss])>1 then error ("(module Calc) shiftR ("++showHS "" r++")\n"++showADL r++"\nin calculation of srcA\n"++show (eqClass order [ source (head cons) | cons<-conss])) else
            foldr1 lub [ source (head cons) | cons<-conss]
     id css = [Tm (mIs c)]
      where a = (source.head.head) css
            c = if not (a `order` b) then error ("(module Calc) shiftR ("++showHS "" r++")\n"++showADL r++"\nass: "++show css++"\nin calculation of c = a `lub` b with a="++show a++" and b="++show b) else
                a `lub` b
            b = (target.last.last) css
     move :: [Expressions] -> [Expressions] -> [([Expressions],[Expressions])]
     move [] css = [([],css)]
     move ass css
      = (ass,css):
        if and [not (null as)| as<-ass]
        then [ts| length (eqClass (==) (map head ass)) == 1
                , sur (multiplicities h)
                , ts<-move (map tail ass) [[flp h]++cs|cs<-css]]++
             [ts| length (eqClass (==) (map last ass)) == 1
                , tot (multiplicities l)
                , ts<-move (map init ass) [cs++[flp l]|cs<-css]]
        else []
        where h=head (map head ass); l=head (map last ass)

   cl::Expression -> Expression
   cl (Fu [F [t]]) = t
   cl (Fu rs)
    | length (eqCl headEq [ts| F ts<-rs])>1 = cr (Fu rs)
    | otherwise = Fu ([ F ([prefix]++[expr])
                      | tss  <- eqCl headEq [ts| F ts<-rs], prefix<-take 1 [head ts| ts<-tss, not (null (tail ts))]
                      , expr <- [cl (Fu [F (tail ts)| ts<-tss, not (null (tail ts))])]
                      ] ++
                      [ expr
                      | tss<-eqCl headEq [ts| F ts<-rs], expr<-take 1 [head ts| ts<-tss, null (tail ts)] ]
                     )
                  where headEq x = (showADL (head x),sign (head x))
   cl (Fi [F [t]]) = t
   cl (Fi rs)
    | length (eqCl headEq [ts| F ts<-rs])>1 = cr (Fi rs)
    | otherwise = Fi ([ F ([prefix]++[expr])
                      | tss  <- eqCl headEq [ts| F ts<-rs], prefix<-take 1 [head ts| ts<-tss, not (null (tail ts))]
                      , expr <- [cl (Fi [F (tail ts)| ts<-tss, not (null (tail ts))])]
                      ] ++
                      [ expr
                      | tss<-eqCl headEq [ts| F ts<-rs], expr<-take 1 [head ts| ts<-tss, null (tail ts)] ]
                     )
                  where headEq x = (showADL (head x),sign (head x))

   cr::Expression -> Expression
   cr (Fu [F [t]]) = t
   cr (Fu rs)
     = Fu ([ F ([expr]++[postfix])
           | tss  <- eqCl lastEq [ts| F ts<-rs], postfix<-take 1 [last ts| ts<-tss, not (null (init ts))]
           , expr <- [cr (Fu [F (init ts)| ts<-tss, not (null (init ts))])]
           ] ++
           [ expr
           | tss<-eqCl lastEq [ts| F ts<-rs], expr<-take 1 [last ts| ts<-tss, null (init ts)] ]
          )
       where lastEq x = (showADL (last x),sign (last x))
   cr (Fi [F [t]]) = t
   cr (Fi rs)
     = Fi ([ F ([expr]++[postfix])
           | tss  <- eqCl lastEq [ts| F ts<-rs], postfix<-take 1 [last ts| ts<-tss, not (null (init ts))]
           , expr <- [cr (Fi [F (init ts)| ts<-tss, not (null (init ts))])]
           ] ++
           [ expr
           | tss<-eqCl lastEq [ts| F ts<-rs], expr<-take 1 [last ts| ts<-tss, null (init ts)] ]
          )
       where lastEq x = (showADL (last x),sign (last x))


  {- Wat gebeurt hier precies???

   data Deduction = Deduce Expression DedNode DedNode
                  | DedAnd Expression DedNode DedNode DedNode
                  | DedOr  Expression DedNode DedNode DedNode
                  | Ded0              DedNode DedNode
                  | Deriv  [Deduction] [DedNode] [DedNode]
                    deriving Eq
   data DedNode   = Dn Prop Expression
   instance Eq DedNode where
    n==n'= showNode n == showNode n'
   mults pat = [Dn p (Tm (makeMph d))|d<-declarations pat, p<-multiplicities d]

   showClause :: Expression -> String
   showClause (Fu terms)
    = sh "/\\" antc++" -: "++sh "\\/" cons
      where
       antc = [t|t<-terms, isNeg t]
       cons = [t|t<-terms, isPos t]
       sh str es = chain str (map showADL es)
   showClause e = error ("(module Calc: function showClause) '"++showADL e++"' has the wrong form and should never occur\nshowHS: "++showHS "" e)

   showMLink (Deduce clause (Dn pl l) (Dn pr r))           = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" implies "++showNode (Dn pr r)
   showMLink (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" and "++showNode (Dn pr r)++" implies "++showNode (Dn pc c)
   showMLink (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = "Deduce ("++showADL clause++")  "++showNode (Dn pl l)++" or " ++showNode (Dn pr r)++" implies "++showNode (Dn pc c)
   showMLink (Ded0          (Dn pl l) (Dn pc c))           = "Ded0 "++showNode (Dn pl l)++" implies "++showNode (Dn pc c)
   showMLink (Deriv clauses fs ts)
    = "Derivation\n     "++showNode (head (froms (head clauses)))++chain "" [showDer c| c<-clauses]
      where
       showDer (Deduce clause (Dn pl l) (Dn pr r))           = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pr r)
       showDer (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pc c)
       showDer (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = "\n  =>  { "++showClause clause++" }\n     "++showNode (Dn pc c)
       showDer (Ded0          (Dn pl l) (Dn pc c))           = "\n  =>\n     "++showNode (Dn pc c)

   froms (Deduce clause (Dn pl l) (Dn pr r))           = [Dn pl l]
   froms (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c)) = [Dn pl l,Dn pr r]
   froms (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c)) = [Dn pl l,Dn pr r]
   froms (Ded0          (Dn pl l) (Dn pc c))           = [Dn pl l]
   froms (Deriv clauses fs ts)                         = fs
   tos (Deduce clause (Dn pl l) (Dn pr r))             = [Dn pr r]
   tos (DedAnd clause (Dn pl l) (Dn pr r) (Dn pc c))   = [Dn pc c]
   tos (DedOr  clause (Dn pl l) (Dn pr r) (Dn pc c))   = [Dn pc c]
   tos (Ded0          (Dn pl l) (Dn pc c))             = [Dn pc c]
   tos (Deriv clauses fs ts)                           = ts
   derivLength (Deriv clauses fs ts)                   = sum (map derivLength clauses)
   derivLength _                                       = 1

   showNode (Dn Uni e) = "univalent("++showADL e++")"
   showNode (Dn Tot e) = "total("++showADL e++")"
   showNode (Dn Inj e) = "injective("++showADL e++")"
   showNode (Dn Sur e) = "surjective("++showADL e++")"
   showNode (Dn x e) = show x++"("++showADL e++")"



   deduce :: [Deduction] -> [Deduction]
   deduce xs
    = f xs' (rd (concat (map froms xs)) `isc` rd (concat (map tos xs)))
       where
        xs' = [ Deriv [d] (froms d) (tos d) | d<-sort' sortCrit xs]
        f q (n:nodes) = f (q `uni` sort' sortCrit [ Deriv [ded,ded'] [a] [b']
                                                  | ded <-q, a <-froms ded,  b <-tos ded,  b==n
                                                  , ded'<-q, a'<-froms ded', b'<-tos ded', a'==n, a/=b']) nodes
        f q []        = q
        sortcrit (Dn p e) = (length (morlist e),show p, showADL e)
        sortCrit d = map sortcrit (tos d++froms d)
        [] `uni` ds' = ds'
        ds `uni` []  = ds
        (d:ds) `uni` (d':ds')
         | sortCrit d<sortCrit d' = d: ds `uni` (d':ds')
         | sortCrit d>sortCrit d' = d': (d:ds) `uni` ds'
         | derivLength d<derivLength d' = d: ds `uni` ds'
         | otherwise = d': ds `uni` ds'

   type DeduceG = [Deduction]
   multDerivations :: Language pat => pat -> DeduceG
   multDerivations pat
    = [ Deriv (flatten deriv) fs ts
      | deriv@(Deriv clauses fs ts)<-(deduce.rd.filter uneq) (fliplinks ++ frs)
      , and[oneMorphism e&&inline m|Dn p e<-fs++ts, m<-morlist e]
      ]
      where
       flatten (Deriv clauses froms tos) = concat (map flatten clauses)
       flatten d = [d]
       frs = concat
             ( [ if idsOnly antc then (if length cons==1 then crule clause (head cons) else []) else
                 if idsOnly cons then (if length antc==1 then arule clause (head antc) else []) else
                 [ Deduce clause (Dn Inj (Fu cons)) (Dn Inj (Fu antc))
                 , Deduce clause (Dn Uni (Fu cons)) (Dn Uni (Fu antc))
                 , Deduce clause (Dn Tot (Fu antc)) (Dn Tot (Fu cons))
                 , Deduce clause (Dn Sur (Fu antc)) (Dn Sur (Fu cons))]
               | rule<-rules pat, conjunct<-conjuncts rule, clause@(Fu terms)<-allClauses conjunct
               , antc<-[[t|t<-terms, isNeg t]]
               , cons<-[[t|t<-terms, isPos t]]
               ])
       crule clause (F ts) = concat [ [Deduce clause (Dn Inj s) (Dn Tot r), Deduce clause (Dn Uni r) (Dn Sur s)] | (s,r)<-split ts ]
       arule clause (F ts) = concat [ [Deduce clause (Dn Tot s) (Dn Inj r), Deduce clause (Dn Sur r) (Dn Uni s)] | (s,r)<-split ts ]
       arule clause x = error ("in module Calc, arule: unexpected pattern: "++showADL x) 
       split []     = []
       split [r]    = []
       split [r,s]  = [(F [r],F [s])]
       split (r:rs) = (F [r],F rs): [(F (r:r'), F (r:s')) | (F r',F s')<-split rs]
 --      uneq (Deduce clause (Dn pa antc) (Dn pc cons)) = pa/=pc || showADL antc/=showADL cons
       uneq ded = null (map showNode (froms ded) `isc` map showNode (tos ded))
       harmonize deduction@(Deduce clause (Dn pa antc) (Dn pc cons))
        = Deduce clause (Dn pa' antc') (Dn pc' cons')
          where
           Dn pa' antc' = if length [a| a<-mors antc, not (inline a)] > length [a| a<-mors antc] `div` 2 
                          then flip (Dn pa antc)
                          else Dn pa antc
           Dn pc' cons' = if length [c| c<-mors cons, not (inline c)] > length [c| c<-mors cons] `div` 2 
                          then flip (Dn pc cons)
                          else Dn pc cons
       flip (Dn p e) = Dn (flipProp p) (flp e)
       fliplinks :: [Deduction]
       fliplinks
        = [ Deduce (Fu [Cp cons, antc']) (Dn pc cons) (Dn pa' antc')
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , Deduce clause' (Dn pa' antc') (Dn pc' cons')<-frs, pc==flipProp pa'&& cons==flp antc'] ++
          [ Ded0 (flip (Dn pa antc)) (Dn pa antc)
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , length [a| a<-mors antc, not (inline a)] > length [a| a<-mors antc] `div` 2] ++
          [ Ded0 (Dn pc cons) (flip (Dn pc cons))
          | Deduce clause (Dn pa antc) (Dn pc cons)<-frs
          , length [c| c<-mors cons, not (inline c)] > length [c| c<-mors cons] `div` 2]
  
   instance Graphic DeduceG where
    dotGraph context style nm links
     = "digraph "++show [x|x<-nm,not(isSpace x)]++introG++newline
             ++ chain newline
                ( [ line "" (Dn pa antc) (Dn pc cons) | link@(Ded0 (Dn pa antc) (Dn pc cons))<-links]++
                  [ line (showADL clause) (Dn pa antc) (Dn pc cons) | link@(Deduce clause (Dn pa antc) (Dn pc cons))<-links])
             ++ "\n   }"
       where
         introG = "\n   { bgcolor=transparent"++newline
               ++ " { node [shape=box,fontsize=18,font=helvetica] "++chain "; " [quote(showNode (Dn p e))| (Dn p e)<-nodes links, oneMorphism e, m<-mors e, inline m, p `elem` multiplicities (makeDeclaration m)]++" }"++newline
               ++ "node [shape=plaintext,fontsize=18,font=helvetica]"++newline
               ++ "edge [fontsize=12,arrowsize=0.8]"
         nodes links = rd ([ Dn pa antc | Deduce clause (Dn pa antc) (Dn pc cons)<-links]++
                           [ Dn pc cons | Deduce clause (Dn pa antc) (Dn pc cons)<-links])
         line s p1 p2 = quote (showNode p1) ++ edgearrow ++ quote (showNode p2) ++if null s then "" else " [label="++quote s++"]"
         --- Nog wat hulpfuncties. Die horen overigens waarschijnlijk niet hier...
         edgearrow = " -> "
         newline = "\n   ; "
         quote s = "\"" ++ s ++ "\" "
  -}
