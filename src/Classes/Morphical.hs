{-# OPTIONS_GHC -Wall #-}
module Classes.Morphical                 (Morphical(concs
                                                   ,mors
                                                   ,morlist
                                                   ,decls
                                                   ,genE
                                                   ,closExprs
                                                   ,idsOnly
                                         )         )
where
   import Adl.Concept                    (Concept(..),Concepts,GenR,Association(..),MorphicId(..))
   import Adl.Context                    (Context(..))
   import Adl.MorphismAndDeclaration     (Morphism(..),Declaration(..),Morphisms,Declarations
                                         ,makeDeclaration,makeInline,mIs)
   import Adl.Gen                        (Gen(..))
   import Adl.Expression                 (Expression(..))
   import Adl.ObjectDef                  (ObjectDef(..))
   import Adl.KeyDef                     (KeyDef(..))
--   import Adl.Population                 (Population(..))
   import Adl.Pattern                    (Pattern(..))
   import Adl.Rule                       (Rule(..),RuleType(..))
   
   import Classification                 (Classification,preCl)
   import Collection                     (Collection(..))
   import Typology                       (genEq,typology)
--   import Classes.Object                 (populations)

   class Morphical a where
    concs        :: a -> Concepts                  -- the set of all concepts used in data structure a
    mors         :: a -> Morphisms                 -- the set of all morphisms used within data structure a
    morlist      :: a -> Morphisms                 -- the list of all morphisms used within data structure a
    decls        :: a -> Declarations              -- all relations used in a. (Don't confuse decls with declarations, which produces the declarations declared in a. The function declarations is bound in ViewPoint)
    decls x       = rd [makeDeclaration m|m<-mors x]
    genE         :: a -> GenR
    genE x        = if null cx then (==) else head cx where cx = [cptgE c|c<-concs x]
    closExprs    :: a -> [Expression]               -- no double occurrences in the resulting list of expressions
    idsOnly      :: a -> Bool
    idsOnly e'    = and [isIdent m'| m'<-mors e'] -- > tells whether all the arguments are equivalent to I

   instance Morphical a => Morphical [a] where
    concs     = rd . concat . map concs
    mors      = rd . concat . map mors
    morlist   =      concat . map morlist
    decls     = rd . concat . map decls
    closExprs = rd . concat . map closExprs

   instance Morphical a => Morphical (Classification a) where
    concs     = rd . concat . map concs     . preCl
    mors      = rd . concat . map mors      . preCl
    morlist   =      concat . map morlist   . preCl
    decls     = rd . concat . map decls     . preCl
    closExprs = rd . concat . map closExprs . preCl


   instance Morphical Context where
    concs     c = concs (ctxds c) `uni` concs (ctxpats c)
    mors      c = mors (ctxpats c) `uni` mors (ctxos c)
    morlist   c = morlist (ctxpats c)++morlist (ctxos c)
    decls     c = decls (ctxrs c) `uni`                 -- the relations used in the rules (outside the scope of patterns)
                  decls (ctxks c) `uni`                 -- the relations used in KeyDefs
                  decls (ctxos c) `uni`                 -- the relations used in ObjDefs
                  rd [d| pat<-ctxpats c, d<-decls pat]  -- the relations used in Patterns
  -- TOELICHTING: de populatie staat nog verspreid over declarations en population statements. In Fspc komen die bij elkaar.
    genE      c = genEq (typology (ctxisa c))
    closExprs c = closExprs (ctxpats c) `uni` closExprs (ctxos c)

   instance Morphical KeyDef where
    concs     kd = [kdcpt kd] `uni` concs (kdats kd)
    mors      kd = mors (kdats kd)
    morlist   kd = morlist (kdats kd)
    genE      kd = genE (kdats kd)
    decls     kd = decls (kdats kd)
    closExprs kd = closExprs (kdats kd)

   instance Morphical Expression where
    concs (Tm mph)     = rd (concs mph)
    concs (Tc f)       = rd (concs f)
    concs (F ts)       = rd (concs ts)
    concs (Fd ts)      = rd (concs ts)
    concs (Fu fs)      = rd (concs fs)
    concs (Fi fs)      = rd (concs fs)
    concs (K0 e')      = rd (concs e')
    concs (K1 e')      = rd (concs e')
    concs (Cp e')      = rd (concs e')

    mors (Tm mph)      = mors (makeInline mph)
    mors (Tc f)        = mors f
    mors (F ts)        = mors ts -- voor a;-b;c hoeft geen extra mors rond b nodig te zijn
    mors (Fd [])       = error ("!Fatal (module Morphical 89): not defined: 'mors (Fd [])'")                    
    mors (Fd ts@(_:t)) = rd (mors ts ++ (concat) [mors (source c)|c<-t])
    mors (Fu fs)       = mors fs
    mors (Fi fs)       = mors fs -- voor a /\ -b hoeft geen extra mors rond b nodig te zijn
    mors (K0 e')       = mors e'
    mors (K1 e')       = mors e'
    mors (Cp e')       = rd (mors e' ++ mors (source e')++mors (target e'))

    morlist (Tm mph)   = morlist mph
    morlist (Tc f)     = morlist f
    morlist (F ts)     = morlist ts
    morlist (Fd ts)    = morlist ts
    morlist (Fu fs)    = morlist fs
    morlist (Fi fs)    = morlist fs
    morlist (K0 e')    = morlist e'
    morlist (K1 e')    = morlist e'
    morlist (Cp e')    = morlist e'

    genE (Tm mph)      = genE mph
    genE (Tc f)        = genE f
    genE (F ts)        = genE ts
    genE (Fd ts)       = genE ts
    genE (Fu fs)       = genE fs
    genE (Fi fs)       = genE fs
    genE (K0 e')       = genE e'
    genE (K1 e')       = genE e'
    genE (Cp e')       = genE e'

    decls (Tm mph)     = decls mph
    decls (Tc f)       = decls f
    decls (F ts)       = decls ts
    decls (Fd ts)      = decls ts
    decls (Fu fs)      = decls fs
    decls (Fi fs)      = decls fs
    decls (K0 e')      = decls e'
    decls (K1 e')      = decls e'
    decls (Cp e')      = decls e'

    closExprs (Tc f)   = closExprs f
    closExprs (F ts)   = (rd.concat.map closExprs) ts
    closExprs (Fd ts)  = (rd.concat.map closExprs) ts
    closExprs (Fu fs)  = (rd.concat.map closExprs) fs
    closExprs (Fi fs)  = (rd.concat.map closExprs) fs
    closExprs (K0 e')  = [K0 e'] `uni` closExprs e'
    closExprs (K1 e')  = [K1 e'] `uni` closExprs e'
    closExprs (Cp e')  = closExprs e'
    closExprs _        = []

   instance Morphical Concept where
    concs c     = [c]
    mors      c = [mIs c]
    morlist   c = [mIs c]
    decls     _ = []
    closExprs _ = []
    genE c = case c of
                C{}        -> cptgE c
                S{}        -> (<=)  ::Concept->Concept->Bool
                Anything   -> (<=)  ::Concept->Concept->Bool
                NOthing    -> (<=)  ::Concept->Concept->Bool

   instance Morphical ObjectDef where
    concs     obj = [source (objctx obj)] `uni` concs (objats obj)
    mors      obj = mors (objctx obj) `uni` mors (objats obj) `uni` mors (target (objctx obj))  -- opletten: de expressie (objctx obj) hoort hier ook bij.
    morlist   obj = morlist (objctx obj)++morlist (objats obj)
    closExprs obj = closExprs (objctx obj) `uni` closExprs (objats obj)

   instance Morphical Morphism where
    concs mph   = rd [source mph,target mph]
    mors mph    = [makeInline mph]
    morlist mph = [mph]
    genE mph    = case mph of
                       Mph{} -> genE (source mph)
                       I{}   -> genE (mphspc mph)
                       V{}   -> genE (source mph)
                       Mp1{} -> error ("!Fatal (module Morphical 163): not defined: 'genE Mp1{}'")
    decls mph   = [makeDeclaration mph]
    closExprs _ = []

   instance Morphical Declaration where
    concs d = case d of
               Sgn{}     -> rd [desrc d,detrg d]
               Isn{}     -> rd [degen d,despc d]
               Iscompl{} -> [despc d]
               Vs{}      -> [despc d]        
    mors _    = []
    morlist _ = []
    genE d    = genE(desrc d)
    decls s   = [s]
    closExprs _ = []

   instance Morphical Pattern where
    concs     pat = concs (ptrls pat) `uni` concs (ptgns pat) `uni` concs (ptdcs pat)
    mors      pat = mors (ptrls pat) `uni` mors (ptkds pat)
    morlist   pat = morlist (ptrls pat)++morlist (ptkds pat)
    genE      pat = genE (ptdcs pat++decls [r| r<-ptrls pat])  
    closExprs pat = closExprs (ptrls pat)

   -- WAAROM??? wordt bij Truth de antecedent niet meegenomen?
   --           Er kunnen toch andere concepten en/of morphismen in de expressies aanwezig zijn in de lhs dan in de rhs??
   -- DAAROM!!! een implicatie is antc |- cons, ofwel -antc\/cons
   --           een truth is      expr        , ofwel -V   \/expr,   ofwel  V |- expr
   --           Daarom laten we de antecedent helemaal weg.
   --           Het systeem genereert zelfs een !Fatal wanneer je naar de antecedent van een Truth zou refereren.
   instance Morphical Rule where
    concs r = case r of
                Ru{rrsrt = Truth } -> concs (rrcon r)
                Ru{}               -> concs (rrant r) `uni` concs (rrcon r)
    mors r = case r of
                Ru{rrsrt = Truth } -> mors (rrcon r)
                Ru{}               -> mors (rrant r) `uni` mors (rrcon r)
    morlist r = case r of
                Ru{rrsrt = Truth } -> morlist (rrcon r)
                Ru{}               -> morlist (rrant r) ++ morlist (rrcon r)
    genE r = case r of
                Ru{rrsrt = Truth } -> genE (rrcon r)
                Ru{}               -> genE [(rrant r),(rrcon r)]
    decls r = case r of
                Ru{rrsrt = Truth } -> decls (rrcon r)
                Ru{}               -> decls (rrant r) `uni` decls (rrcon r)
    closExprs r = case r of
                Ru{rrsrt = Truth } -> closExprs (rrcon r)
                Ru{}               -> closExprs (rrant r) `uni` closExprs (rrcon r)

   instance Morphical Gen where
    concs g     = rd [gengen g,genspc g]  
    mors g      = [I{ mphats=[]
                    , mphgen = gengen g
                    , mphspc = genspc g
                    , mphyin = True
                    }]                         
    morlist g   = mors g
    genE g      = genE (genspc g)
    decls     _ = []
    closExprs _ = []


    