module Ampersand.FSpec.ToFSpec.ADL2FSpec
         (makeFSpec) where
import Prelude
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (pack)
import Ampersand.ADL1
import Ampersand.Basics
import Ampersand.Classes
import Ampersand.Core.ParseTree
     ( Role
     )
import Ampersand.Core.AbstractSyntaxTree
import Ampersand.FSpec.FSpec
import Ampersand.Misc
import Ampersand.FSpec.Crud
import Ampersand.FSpec.ToFSpec.ADL2Plug
import Ampersand.FSpec.ToFSpec.Calc
import Ampersand.FSpec.ToFSpec.NormalForms 
import Ampersand.FSpec.ToFSpec.Populated 
import Ampersand.Core.ShowAStruct

{- The FSpec-datastructure should contain all "difficult" computations. This data structure is used by all sorts of rendering-engines,
such as the code generator, the functional-specification generator, and future extentions. -}
makeFSpec :: Options -> A_Context -> FSpec
makeFSpec opts context
 =      FSpec { fsName       = pack (name context)
              , originalContext = context 
              , getOpts      = opts
              , fspos        = ctxpos context
              , themes       = themesInScope
              , pattsInScope = pattsInThemesInScope
              , rulesInScope = rulesInThemesInScope
              , declsInScope = declsInThemesInScope 
              , concsInScope = concsInThemesInScope
              , cDefsInScope = cDefsInThemesInScope
              , gensInScope  = gensInThemesInScope
              , fsLang       = printingLanguage
              , vplugInfos   = definedplugs
              , plugInfos    = allplugs
              , interfaceS   = fSpecAllInterfaces -- interfaces specified in the Ampersand script
              , roleInterfaces = fSpecRoleInterfaces
              , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objExpression (ifcObj ifc)
                                    , isIdent ctxrel && source ctxrel==ONE
                                      || ctxrel `notElem` map (objExpression.ifcObj) fSpecAllInterfaces
                                    , allInterfaces opts]  -- generated interfaces
              , fDeriveProofs = deriveProofs opts context 
              , fRoleRels    = nub [(role',decl) -- fRoleRels says which roles may change the population of which relation.
                                   | rr <- ctxRRels context
                                   , decl <- rrRels rr
                                   , role' <- rrRoles rr
                                   ] 
              , fRoleRuls    = nub [(role',rule)   -- fRoleRuls says which roles maintain which rules.
                                   | rule <- allrules
                                   , role' <- maintainersOf rule
                                   ]
              , fMaintains   = fMaintains'
              , fRoles       = zip ((sort . nub) (concatMap arRoles (ctxrrules context)++
                                                  concatMap rrRoles (ctxRRels context )++
                                                  concatMap ifcRoles (ctxifcs context )
                                                 )
                                   ) [0..] 
              , fallRules    = allrules
              , vrules       = filter      isUserDefined  allrules
              , grules       = filter (not.isUserDefined) allrules
              , invariants   = filter (not.isSignal)      allrules
              , signals      = filter      isSignal       allrules
              , allConjuncts = allConjs
              , allConjsPerRule = fSpecAllConjsPerRule
              , allConjsPerDecl = fSpecAllConjsPerDecl
              , allConjsPerConcept = fSpecAllConjsPerConcept
              , vquads       = allQuads
              , allUsedDecls = relsUsedIn context
              , vrels        = calculatedDecls
              , allConcepts  = fSpecAllConcepts
              , cptTType     = representationOf contextinfo
              , fsisa        = nub . concatMap genericAndSpecifics . gens $ context
              , vpatterns    = patterns context
              , vgens        = gens context
              , vIndices     = identities context
              , vviews       = viewDefs context
              , lookupView   = lookupView'
              , getDefaultViewForConcept = getDefaultViewForConcept'
              , getAllViewsForConcept = getAllViewsForConcept'
              , conceptDefs  = ctxcds context
              , fSexpls      = ctxps context
              , metas        = ctxmetas context
              , crudInfo     = mkCrudInfo fSpecAllConcepts calculatedDecls fSpecAllInterfaces
              , atomsInCptIncludingSmaller = atomValuesOf contextinfo initialpopsDefinedInScript --TODO: Write in a nicer way, like `atomsBySmallestConcept`
              , atomsBySmallestConcept = \cpt -> map apLeft . pairsinexpr 
                                               . foldl (.-.) (EDcI cpt) 
                                               . map (handleType cpt)
                                               . smallerConcepts (gens context) $ cpt
              , tableContents = tblcontents contextinfo initialpopsDefinedInScript
              , pairsInExpr  = pairsinexpr
              , allViolations  = [ (r,vs)
                                 | r <- allrules -- Removed following, because also violations of invariant rules are violations.. , not (isSignal r)
                                 , let vs = ruleviolations r, not (null vs) ]
              , allExprs     = expressionsIn context `uni` expressionsIn allConjs
              , initialConjunctSignals = [ (conj, viols) | conj <- allConjs 
                                         , let viols = conjunctViolations conj
                                         , not $ null viols
                                         ]
              , fcontextInfo = contextinfo
              , ftypologies   = typologies context
              , typologyOf = typologyOf'
              , largestConcept = getLargestConcept 
              , specializationsOf = smallerConcepts (gens context)
              , generalizationsOf = largerConcepts  (gens context)
              }
   where           
     getLargestConcept cpt = case largerConcepts (gens context) cpt of
                              [] -> cpt
                              x:_ -> getLargestConcept x
     handleType :: A_Concept -> A_Concept -> Expression
     handleType gen spc = EEps gen (Sign gen spc) .:. EDcI spc .:. EEps gen (Sign spc gen)
     fMaintains' :: Role -> [Rule]
     fMaintains' role' = nub [ rule 
                            | rule <- allrules
                            , role' `elem` maintainersOf rule
                            ]
     typologyOf' cpt = 
        case [t | t <- typologies context, cpt `elem` tyCpts t] of
           [t] -> t
           _   -> fatal ("concept "++name cpt++" should be in exactly one typology!")
     pairsinexpr  :: Expression -> [AAtomPair]
     pairsinexpr = fullContents contextinfo initialpopsDefinedInScript
     ruleviolations :: Rule -> [AAtomPair]
     ruleviolations r = case formalExpression r of
          EEqu{} -> (cra >- crc) ++ (crc >- cra)
          EInc{} -> cra >- crc
          _      -> pairsinexpr (EDcV (sign (consequent r))) >- crc  --everything not in con
          where cra = pairsinexpr (antecedent r)
                crc = pairsinexpr (consequent r)
     conjunctViolations :: Conjunct -> [AAtomPair]
     conjunctViolations conj = pairsinexpr (notCpl (rc_conjunct conj))
     contextinfo = ctxInfo context

     fSpecAllConcepts = concs context
     fSpecAllInterfaces :: [Interface]
     fSpecAllInterfaces = map enrichIfc (ctxifcs context)
       where
          enrichIfc :: Interface -> Interface
          enrichIfc ifc
           = ifc{ ifcControls = makeIfcControls [] allConjs
                }
     fSpecRoleInterfaces :: Role -> [Interface]
     fSpecRoleInterfaces role' = filter (forThisRole role') fSpecAllInterfaces
     forThisRole ::Role -> Interface -> Bool
     forThisRole role' interf = case ifcRoles interf of
                                     []   -> True -- interface is for all roles
                                     rs  -> role' `elem` rs
     
     themesInScope = if null (ctxthms context)   -- The names of patterns/processes to be printed in the functional design document. (for making partial documentation)
                     then map name (patterns context)
                     else ctxthms context
     pattsInThemesInScope = filter (\p -> name p `elem` themesInScope) (patterns context)
     cDefsInThemesInScope = filter (\cd -> cdfrom cd `elem` themesInScope) (ctxcds context)
     rulesInThemesInScope = ctxrs context `uni` nub (concatMap ptrls pattsInThemesInScope)
     declsInThemesInScope = ctxds context `uni` nub (concatMap ptdcs pattsInThemesInScope)
     concsInThemesInScope = concs (ctxrs context) `uni`  concs pattsInThemesInScope
     gensInThemesInScope  = nub (ctxgs context ++ concatMap ptgns pattsInThemesInScope)

     initialpopsDefinedInScript = 
                   [ let dcl = popdcl (head eqclass)
                     in ARelPopu{ popsrc = source dcl
                                , poptgt = target dcl
                                , popdcl = dcl
                                , popps  = (nub.concat) [ popps pop | pop<-eqclass ]
                                }
                   | eqclass<-eqCl popdcl [ pop | pop@ARelPopu{}<-populations ] ] ++
                   [ ACptPopu{ popcpt = popcpt (head eqclass)
                             , popas  = (nub.concat) [ popas pop | pop<-eqclass ]
                             }
                   | eqclass<-eqCl popcpt [ pop | pop@ACptPopu{}<-populations ] ]
       where populations = ctxpopus context++concatMap ptups (patterns context)       
     allConjs = makeAllConjs opts allrules
     fSpecAllConjsPerRule :: [(Rule,[Conjunct])]
     fSpecAllConjsPerRule = converse [ (conj, rc_orgRules conj) | conj <- allConjs ]
     fSpecAllConjsPerDecl = converse [ (conj, relsUsedIn $ rc_conjunct conj) | conj <- allConjs ] 
     fSpecAllConjsPerConcept = converse [ (conj, smaller (source r) `uni` smaller (target r)) | conj <- allConjs, r <- relsMentionedIn $ rc_conjunct conj ]
                               where smaller :: A_Concept -> [A_Concept]
                                     smaller cpt = [cpt] `uni` smallerConcepts (gens context) cpt
     allQuads = quadsOfRules opts allrules 
     
     allrules = map setIsSignal (allRules context)
        where setIsSignal r = r{isSignal = (not.null) (maintainersOf r)}
     maintainersOf :: Rule -> [Role]
     maintainersOf r 
       = concatMap arRoles . filter forThisRule . ctxrrules $ context
         where
          forThisRule :: A_RoleRule -> Bool
          forThisRule x = name r `elem` arRules x
     isUserDefined rul =
       case r_usr rul of
         UserDefined  -> True
         Multiplicity -> False
         Identity     -> False
     calcProps :: Relation -> Relation
     calcProps d = d{decprps_calc = Just calculated}
         where calculated = decprps d `uni` [Tot | d `elem` totals]
                                      `uni` [Sur | d `elem` surjectives]
     calculatedDecls = map calcProps (relsDefdIn context)
  -- determine relations that are total (as many as possible, but not necessarily all)
     totals      = [ d |       EDcD d  <- totsurs ]
     surjectives = [ d | EFlp (EDcD d) <- totsurs ]
     totsurs :: [Expression]
     totsurs
      = nub [rel | q<-filter (not . isSignal . qRule) allQuads -- all quads for invariant rules
                 , isIdent (qDcl q)
                 , x<-qConjuncts q, dnf<-rc_dnfClauses x
                 , let antc = conjNF opts (foldr (./\.) (EDcV (sign (head (antcs dnf++conss dnf)))) (antcs dnf))
                 , isRfx antc -- We now know that I is a subset of the antecedent of this dnf clause.
                 , cons<-map exprCps2list (conss dnf)
            -- let I |- r;s;t be an invariant rule, then r and s and t~ and s~ are all total.
                 , rel<-init cons++[flp r | r<-tail cons]
                 ]
  -- Lookup view by id in fSpec.
     lookupView' :: String -> ViewDef
     lookupView'  viewId =
       case filter (\v -> name v == viewId) $ viewDefs context of
         []   -> fatal ("Undeclared view " ++ show viewId ++ ".") -- Will be caught by static analysis
         [vd] -> vd
         vds  -> fatal ("Multiple views with id " ++ show viewId ++ ": " ++ show (map name vds)) -- Will be caught by static analysis
     
   -- get all views for a specific concept and all larger concepts.
     getAllViewsForConcept' :: A_Concept -> [ViewDef]
     getAllViewsForConcept' concpt = 
              concatMap viewsOfThisConcept
            . sortSpecific2Generic (gens context) 
            $ concpt : largerConcepts (gens context) concpt 
       
     viewsOfThisConcept :: A_Concept -> [ViewDef]
     viewsOfThisConcept cpt = filter isForConcept $ viewDefs context
       where
         isForConcept :: ViewDef -> Bool
         isForConcept vd = vdcpt vd == cpt
     -- Return the default view for cpt, which is either the view for cpt itself (if it has one) or the view for
     -- cpt's smallest superconcept that has a default view. Return Nothing if there is no default view.
     getDefaultViewForConcept' :: A_Concept -> Maybe ViewDef
     getDefaultViewForConcept' cpt =
       case  filter vdIsDefault
           . concatMap viewsOfThisConcept
           . sortSpecific2Generic (gens context) 
           $ cpt : largerConcepts (gens context) cpt of
         []     -> Nothing 
         (vd:_) -> Just vd

     --------------
     --making plugs
     --------------
     vsqlplugs = case ctxsql context of
                   []  -> []
                   _   -> fatal "User defined plugs are heavily bitrotted." --REMARK -> no optimization like try2specific, because these plugs are user defined
     definedplugs = map InternalPlug vsqlplugs
                 ++ map ExternalPlug (ctxphp context)
     allplugs = definedplugs ++      -- all plugs defined by the user
                genPlugs             -- all generated plugs
     genPlugs = [InternalPlug (rename p (qlfname (name p)))
                | p <- uniqueNames (map name definedplugs) -- the names of definedplugs will not be changed, assuming they are all unique
                                   (makeGeneratedSqlPlugs opts context calcProps)
                ]
     qlfname x = if null (namespace opts) then x else "ns"++namespace opts++x

     --TODO151210 -> Plug A is overbodig, want A zit al in plug r
--CONTEXT Temp
--PATTERN Temp
--r::A*B[TOT].
--t::E*ECps[UNI].
--ENDPATTERN
--ENDCONTEXT
{-
    **************************************
    * Plug E                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * t  [UNI]                             *
    **************************************
    * Plug ECps                            *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug B                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug A                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug r                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * r  [TOT]                             *
    **************************************
-}
     -------------------
     --END: making plugs
     -------------------
     -------------------
     --making interfaces
     -------------------
     -- interfaces (type ObjectDef) can be generated from a basic ontology. That is: they can be derived from a set
     -- of relations together with multiplicity constraints. That is what interfaceG does.
     -- This is meant to help a developer to build his own list of interfaces, by providing a set of interfaces that works.
     -- The developer may relabel attributes by names of his own choice.
     -- This is easier than to invent a set of interfaces from scratch.

     -- Rule: a interface must be large enough to allow the required transactions to take place within that interface.
     -- Attributes of an ObjectDef have unique names within that ObjectDef.

--- generation of interfaces:
--  Ampersand generates interfaces for the purpose of quick prototyping.
--  A script without any mention of interfaces is supplemented
--  by a number of interface definitions that gives a user full access to all data.
--  Step 1: select and arrange all relations to obtain a set cRels of total relations
--          to ensure insertability of entities (signal relations are excluded)
     cRels = [     EDcD d  | d<-calculatedDecls, isTot d, not$decplug d]++
             [flp (EDcD d) | d<-calculatedDecls, not (isTot d) && isSur d, not$decplug d]
--  Step 2: select and arrange all relations to obtain a set dRels of injective relations
--          to ensure deletability of entities (signal relations are excluded)
     dRels = [     EDcD d  | d<-calculatedDecls, isInj d, not$decplug d]++
             [flp (EDcD d) | d<-calculatedDecls, not (isInj d) && isUni d, not$decplug d]
--  Step 3: compute longest sequences of total expressions and longest sequences of injective expressions.
     maxTotPaths = map (:[]) cRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     maxInjPaths = map (:[]) dRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     --    Warshall's transitive closure algorithm, adapted for this purpose:
--     clos1 :: [Expression] -> [[Expression]]
--     clos1 xs
--      = foldl f [ [ x ] | x<-xs] (nub (map source xs) `isc` nub (map target xs))
--        where
--          f :: [[Expression]] -> A_Concept -> [[Expression]]
--          f q x = q ++ [l ++ r | l <- q, x == target (last l),
--                                 r <- q, x == source (head r), null (l `isc` r)]

--  Step 4: i) generate interfaces starting with INTERFACE concept: I[Concept]
--          ii) generate interfaces starting with INTERFACE concepts: V[ONE*Concept]
--          note: based on a theme one can pick a certain set of generated interfaces (there is not one correct set)
--                default theme => generate interfaces from the clos total expressions and clos injective expressions (see step1-3).
--                student theme => generate interface for each concept with relations where concept is source or target (note: step1-3 are skipped)
     interfaceGen = step4a ++ step4b
     step4a
      = let recur es
             = [ Obj { objnm   = showA t
                     , objpos  = Origin "generated recur object: step 4a - default theme"
                     , objExpression  = t
                     , objcrud = fatal "No default crud in generated interface"
                     , objmView = Nothing
                     , objmsub = Just . Box (target t) Nothing $ recur [ pth | (_:pth)<-cl, not (null pth) ]
                     }
               | cl<-eqCl head es, (t:_)<-take 1 cl] --
            -- es is a list of expression lists, each with at least one expression in it. They all have the same source concept (i.e. source.head)
            -- Each expression list represents a path from the origin of a box to the attribute.
            -- 16 Aug 2011: (recur es) is applied once where es originates from (maxTotPaths `uni` maxInjPaths) both based on clos
            -- Interfaces for I[Concept] are generated only for concepts that have been analysed to be an entity.
            -- These concepts are collected in gPlugConcepts
            gPlugConcepts = [ c | InternalPlug plug@TblSQL{}<-genPlugs , (c,_)<-take 1 (cLkpTbl plug) ]
            -- Each interface gets all attributes that are required to create and delete the object.
            -- All total attributes must be included, because the interface must allow an object to be deleted.
        in
        [Ifc { ifcname     = name c
             , ifcObj      = Obj { objnm   = name c
                                 , objpos  = Origin "generated object: step 4a - default theme"
                                 , objExpression  = EDcI c
                                 , objcrud = fatal "No default crud in generated interface"
                                 , objmView = Nothing
                                 , objmsub = Just . Box c Nothing $ objattributes
                                 }
             , ifcControls = makeIfcControls params allConjs
             , ifcPos      = Origin "generated interface: step 4a - default theme"
             , ifcPrp      = "Interface " ++name c++" has been generated by Ampersand."
             , ifcRoles    = []
             }
        | cl <- eqCl (source.head) [ pth | pth<-maxTotPaths `uni` maxInjPaths, (source.head) pth `elem` gPlugConcepts ]
        , let objattributes = recur cl
        , not (null objattributes) --de meeste plugs hebben in ieder geval I als attribuut
        , --exclude concept A without cRels or dRels (i.e. A in Scalar without total associations to other plugs)
          not (length objattributes==1 && isIdent(objExpression(head objattributes)))
        , let e0=head cl, not (null e0) || fatal "null e0"
        , let c=source (head e0)
        , let params = [ d | EDcD d <- concatMap primsMentionedIn (expressionsIn objattributes)]
        ]
     --end otherwise: default theme
     --end stap4a
     step4b --generate lists of concept instances for those concepts that have a generated INTERFACE in step4a
      = [Ifc { ifcname     = nm
             , ifcObj      = Obj { objnm   = nm
                                 , objpos  = Origin "generated object: step 4b"
                                 , objExpression  = EDcI ONE
                                 , objcrud = fatal "No default crud in generated interface"
                                 , objmView = Nothing
                                 , objmsub = Just . Box ONE Nothing $ [att]
                                 }
             , ifcControls = ifcControls ifcc
             , ifcPos      = ifcPos      ifcc
             , ifcPrp      = ifcPrp      ifcc
             , ifcRoles    = []
             }
        | ifcc<-step4a
        , let c   = source(objExpression (ifcObj ifcc))
              nm'::Int->String
              nm' 0  = plural printingLanguage (name c)
              nm' i  = plural printingLanguage (name c) ++ show i
              nms = [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)]
              nm
                | null nms = fatal "impossible"
                | otherwise = head nms
              att = Obj { objnm    = name c
                        , objpos   = Origin "generated attribute object: step 4b"
                        , objExpression   = EDcV (Sign ONE c)
                        , objcrud  = fatal "No default crud in generated interface."
                        , objmView = Nothing
                        , objmsub  = Nothing
                        }
        ]
     ----------------------
     --END: making interfaces
     ----------------------
     printingLanguage = fromMaybe (ctxlang context) (language opts)  -- The language for printing this specification is taken from the command line options (language opts). If none is specified, the specification is printed in the language in which the context was defined (ctxlang context).

makeIfcControls :: [Relation] -> [Conjunct] -> [Conjunct]
makeIfcControls params allConjs
 = [ conj 
   | conj<-allConjs
   , (not.null) (map EDcD params `isc` primsMentionedIn (rc_conjunct conj))
   -- Filtering for uni/inj invariants is pointless here, as we can only filter out those conjuncts for which all
   -- originating rules are uni/inj invariants. Conjuncts that also have other originating rules need to be included
   -- and the uni/inj invariant rules need to be filtered out at a later stage (in Generate.hs).
   ]
  

class Named a => Rename a where
 rename :: a->String->a
 -- | the function uniqueNames ensures case-insensitive unique names like sql plug names
 uniqueNames :: [String]->[a]->[a]
 uniqueNames taken xs
  = [p | cl<-eqCl (map toLower.name) xs  -- each equivalence class cl contains (identified a) with the same map toLower (name p)
       , p <-if name (head cl) `elem` taken || length cl>1
             then [rename p (name p++show i) | (p,i)<-zip cl [(1::Int)..]]
             else cl
    ]

instance Rename PlugSQL where
 rename p x = p{sqlname=x}
     

tblcontents :: ContextInfo -> [Population] -> PlugSQL -> [[Maybe AAtomValue]]
tblcontents ci ps plug
   = case plug of
     BinSQL{}    -> let expr = case dLkpTbl plug of
                                 [store] -> EDcD (rsDcl store)
                                 ss       -> fatal ("Exactly one relation sould be stored in BinSQL. However, there are "++show (length ss))
                    in [[(Just . apLeft) p,(Just . apRight) p] |p<-fullContents ci ps expr]
     TblSQL{}    -> 
 --TODO15122010 -> remove the assumptions (see comment data PlugSQL)
 --attributes are assumed to be in the order kernel+other,
 --where NULL in a kernel attribute implies NULL in the following kernel attributes
 --and the first attribute is unique and not null
 --(r,s,t)<-mLkpTbl: s is assumed to be in the kernel, attExpr t is expected to hold r or (flp r), s and t are assumed to be different
        case attributes plug of 
         []   -> fatal "no attributes in plug."
         f:fs -> (nub.transpose)
                 ( map Just cAtoms
                 : [case fExp of
                       EDcI c -> [ if a `elem` atomValuesOf ci ps c then Just a else Nothing | a<-cAtoms ]
                       _      -> [ (lkp att a . fullContents ci ps) fExp | a<-cAtoms ]
                   | att<-fs, let fExp=attExpr att
                   ]
                 )
                 where
                   cAtoms = (atomValuesOf ci ps. source . attExpr) f
                   lkp :: SqlAttribute -> AAtomValue -> [AAtomPair] -> Maybe AAtomValue
                   lkp att a pairs
                    = case [ p | p<-pairs, a==apLeft p ] of
                       [] -> Nothing
                       [p] -> Just (apRight p)
                       ps' -> fatal . unlines $ 
                                [ "There is an attempt to populate multiple values into "
                                , "     the row of table `"++name plug++"`, where id = "++show(showValADL a)++":"
                                , "     Values to be inserted in field `"++name att++"` are: "++show (map (showValADL . apRight) ps')
                                ] --this has happend before due to:
                                  --    when using --dev flag
                                  --  , when there are violations
                                  --  , when you have INCLUDE \"MinimalAST.xlsx\" in formalampersand.)
                                  --  , when a relation in formalAmpersand is declared UNI, but actually it isn't.

                               
                        
                        