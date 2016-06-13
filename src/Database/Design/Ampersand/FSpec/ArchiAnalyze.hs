{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}
module Database.Design.Ampersand.FSpec.ArchiAnalyze (archi2PContext)
   -- The purpose of this module is to load Archimate content into an Ampersand context.
   -- This module parses an Archi-repository by means of function `archi2PContext`, which produces a `P_Context` for merging into Ampersand.
   -- That `P_Context` contains both the Archimate-metamodel (in the form of declarations) and the Archimate population that represents the model.
   -- The fact that Archimate produces a mix of model and metamodel is therefore not a problem.
where
   import Database.Design.Ampersand.Basics (fatal, eqCl)
   import Data.Char                         -- for things such as toLower
   import Data.List                         -- for things such as intercalate
   import qualified Data.Map.Strict as Map  -- import qualified, to avoid name clashes with Prelude functions
   import Data.Tree.NTree.TypeDefs
   import Text.XML.HXT.Core hiding (utf8, fatal)
   import Database.Design.Ampersand.Core.ParseTree


   -- | archi2PContext is meant to grind the contents of an Archi-repository into declarations and population inside a fresh Ampersand P_Context.
   --   The process starts by parsing an XML-file by means of function `processStraight` into a data structure called `archiRepo`.
   --   From this datastructure, we can derive a lookup-function, called `typeLookup`.
   --   It assigns the Archi-type (e.g. Business Process) to an arbitrary Archi-identifier (e.g. "0957873").
   --   Then, the properties have to be provided with identifiers (see class `WithProperties`), because Archi represents them just as key-value pairs.
   --   The function `grindArchiPop` retrieves the population of meta-relations
   --   It produces the P_Populations and P_Declarations that represent the Archimate model.
   --   Finally, the function `mkArchiContext` produces a `P_Context` ready to be merged into the rest of Ampersand's population.
   archi2PContext :: IO P_Context
   archi2PContext
    = do archiRepo <- runX (processStraight "CA repository.xml")
         let typeLookup atom = (Map.fromList . typeMap) archiRepo Map.! atom
         return ((mkArchiContext . grindArchiPop typeLookup . identifyProps []) archiRepo)

   mkArchiContext :: [(P_Population,P_Declaration)] -> P_Context
   mkArchiContext pops =
     PCtx{ ctx_nm     = "Archimate"
         , ctx_pos    = []
         , ctx_lang   = fatal 686 "No language because of Archi-import hack. Please report this as a bug"
         , ctx_markup = Nothing
         , ctx_thms   = []
         , ctx_pats   = []
         , ctx_rs     = []
         , ctx_ds     = archiDecls
         , ctx_cs     = []
         , ctx_ks     = []
         , ctx_rrules = []
         , ctx_rrels  = []
         , ctx_reprs  = []
         , ctx_vs     = []
         , ctx_gs     = []
         , ctx_ifcs   = []
         , ctx_ps     = []
         , ctx_pops   = archiPops
         , ctx_sql    = []
         , ctx_php    = []
         , ctx_metas  = []
         }
     where equivClasses :: [[(P_Population, P_Declaration)]]
           equivClasses = eqCl snd pops
           archiPops  = [ (foldr1 mergePop.map fst) cl | cl<-equivClasses ]
           archiDecls = [ (head.nub.map snd) cl | cl<-equivClasses ]
           mergePop pop0 pop1 = pop0{p_popps = xs++[y | y<-ys, y `notElem` xs]}
            where xs = p_popps pop0
                  ys = p_popps pop1


-- The following code defines a data structure (called ArchiRepo) that corresponds to an Archi-repository in XML.

   data ArchiRepo = ArchiRepo
     { archRepoName   :: String
     , archRepoId     :: String
     , archFolders    :: [Folder]
     , archProperties :: [ArchiProp]
     } deriving (Show, Eq)
 
   data Folder = Folder
     { fldName        :: String
     , fldId          :: String
     , fldType        :: String
     , fldElems       :: [Element]
     , fldFolders     :: [Folder]
     } deriving (Show, Eq)

   data Element = Element
     { elemType       :: String
     , elemId         :: String
     , elemName       :: String
     , elemSrc        :: String
     , elemTgt        :: String
     , elemDocu       :: String
     , elChilds       :: [Child]
     , elProps        :: [ArchiProp]
     } deriving (Show, Eq)

-- Children occur in views only.
   data Child = Child
     { chldType       :: String
     , chldId         :: String
     , chldAlgn       :: String
     , chldFCol       :: String
     , chldElem       :: String
     , trgtConn       :: String
     , bound          :: Bound
     , srcConns       :: [SourceConnection]
     , childs         :: [Child]
     } deriving (Show, Eq)

   data Relation = Relation
     { relType        :: String
     , relHref        :: String
     } deriving (Show, Eq)

   data Bound = Bound
     { bnd_x          :: String
     , bnd_y          :: String
     , bnd_width      :: String
     , bnd_height     :: String
     } deriving (Show, Eq)

   data SourceConnection = SrcConn
     { sConType       :: String
     , sConId         :: String
     , sConSrc        :: String
     , sConTgt        :: String
     , sConRel        :: String
     , sConRelat      :: [Relation]
     , sCbendPts      :: [BendPoint]
     } deriving (Show, Eq)

   data BendPoint = BendPt
     { bpStartX       :: String
     , bpStartY       :: String
     , bpEndX         :: String
     , bpEndY         :: String
     } deriving (Show, Eq)

   data ArchiProp = ArchiProp
     { archPropId     :: String
     , archPropKey    :: String
     , archPropVal    :: String
     } deriving (Show, Eq)


-- | Properties in Archimate have no identifying key. In Ampersand, that key is necessary. So the class WithProperties is defined to
--   generate keys for properties, to be inserted in the grinding process. The only data structures with properties in the inner structure
--   of Archi (i.e. in the repository minus the Views), are folders and elements. For this reason, the types ArchiRepo, Folder, and Element
--   are instances of class WithProperties.

   class WithProperties a where
     allProps      :: a -> [ArchiProp]        -- takes all properties from an ArchiRepo, a Folder, or an Element
     identifyProps :: [String] -> a -> a -- distributes identifiers ( [String] ) over an ArchiRepo, a Folder, or an Element, in order to assign a unique identifier to each property in it.

   instance WithProperties ArchiRepo where
     allProps archiRepo = allProps (archFolders archiRepo) ++ archProperties archiRepo
     identifyProps _ archiRepo = archiRepo
       { archProperties = [ prop{archPropId=propId} | (prop,propId)<- zip (archProperties archiRepo) propIds ]
       , archFolders    = identifyProps fldrIds (archFolders archiRepo)
       }
       where
         identifiers = [ "pr-"++show (i::Integer) | i<-[0..] ] -- infinitely many unique keys to identify properties.
         fldrIds = take ((length.allProps.archFolders) archiRepo) identifiers
         propIds = drop ((length.allProps.archFolders) archiRepo) identifiers

   instance WithProperties Folder where
     allProps folder = allProps (fldElems folder) ++ allProps (fldFolders folder)
     identifyProps identifiers folder = folder
       { fldElems   = identifyProps elemsIdentifiers (fldElems folder)
       , fldFolders = identifyProps foldersIdentifiers (fldFolders folder)
       }
       where
         elemsIdentifiers   = take ((length.allProps.fldElems) folder) identifiers
         foldersIdentifiers = drop ((length.allProps.fldElems) folder) identifiers

   instance WithProperties Element where
     allProps element = elProps element
--                      ++ allProps (elChilds element)   -- children are not (yet) being analyzed, so we skip the elChilds of the element.
     identifyProps identifiers element = element
       { elProps = [ prop{archPropId=propId} | (propId,prop)<- zip identifiers (elProps element) ] }

   instance WithProperties a => WithProperties [a] where
     allProps xs = concatMap allProps xs
     identifyProps identifiers xs
      = [ identifyProps ids x | (ids,x) <- zip idss xs ]
        where
         countProperties :: [Int] -- a list that contains the lengths of property lists in `folder`
         countProperties = map (length.allProps) xs
         idss = distr countProperties identifiers
         distr :: [Int] -> [a] -> [[a]]  -- distribute identifiers in order to allocate them to items in `archiRepo`
         distr (n:ns) idents = take n idents: distr ns (drop n idents)
         distr []     _      = []


-- | In order to populate an Archi-metamodel with the contents of an Archi-repository,
--   we must grind that contents into binary tables. For that purpose, we define the
--   class MetaArchi, and instantiate it on ArchiRepo and all its constituent types.
   class MetaArchi a where
     typeMap ::        a -> [(String,String)]     -- the map that determines the type (xsi:type) of every atom (id-field) in the repository
     grindArchiPop :: (String->String) -> a -> [(P_Population,P_Declaration)] -- create population and the corresponding metamodel for the metamodel of Archi (i.e. BusinessProcess, DataObject, etc.)
     keyArchi ::       a -> String                -- get the key value (dirty identifier) of an a.

   instance MetaArchi ArchiRepo where
     typeMap archiRepo
      = typeMap [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]  ++ 
        (typeMap.archProperties) archiRepo
     grindArchiPop typeLookup archiRepo
      = (concat.map (grindArchiPop typeLookup)) backendFolders  ++ 
        (concat.map (grindArchiPop typeLookup).archProperties) archiRepo
        where backendFolders = [ folder | folder<-archFolders archiRepo, fldName folder/="Views"]
     keyArchi = archRepoId

   instance MetaArchi Folder where
     typeMap folder
      = (typeMap.fldElems)   folder  ++ 
        (typeMap.fldFolders) folder
     grindArchiPop typeLookup folder
      = (concat.map (grindArchiPop typeLookup).fldElems)   folder  ++ 
        (concat.map (grindArchiPop typeLookup).fldFolders) folder
     keyArchi = fldId

   instance MetaArchi Element where
     typeMap element
      = [(elemId element, elemType element) | (not.null.elemName) element, (null.elemSrc) element]    
         ++ typeMap (elProps element)
     grindArchiPop typeLookup element
      = [ transform typeLookup "name" (elemType element) [(elemId element, elemName element)]
        | (not.null.elemName) element, (null.elemSrc) element] ++
        [ transform typeLookup "docu" (elemType element) [(elemId element, elemDocu element)]
        | (not.null.elemDocu) element, (null.elemSrc) element] ++
        [ transform typeLookup "relationship" (elemType element) [(elemSrc element, elemTgt element)]
        | (null.elemName) element, (not.null.elemSrc) element] ++
        [ transform typeLookup (elemName element) (elemType element) [(elemSrc element, elemTgt element)]
        | (not.null.elemName) element, (not.null.elemSrc) element] ++
        (concat.map (grindArchiPop typeLookup).elProps) element
     keyArchi = elemId

   instance MetaArchi ArchiProp where
     typeMap _
      = []
     grindArchiPop typeLookup property
      = [ transform typeLookup "key" "Property"
            [(keyArchi property, archPropKey property) | (not.null.archPropKey) property ]
        , transform typeLookup "value" "Property"
            [(keyArchi property, archPropVal property) | (not.null.archPropVal) property ]
        ]
     keyArchi = archPropId

   instance MetaArchi a => MetaArchi [a] where
     typeMap                  xs = concat [ typeMap                  x | x<-xs ]
     grindArchiPop typeLookup xs = concat [ grindArchiPop typeLookup x | x<-xs ]
     keyArchi = error "fatal 269: cannot use keyArchi on a list"

-- | The function `transform` does the actual compilation of data elements of archiRepo into the Ampersand structure.
--   It looks redundant to produce both a `P_Population` and a `P_Declaration`, but the first contains the population and the second is used to
--   include the metamodel of Archimate in the population. This save the author the effort of maintaining an Archimate-metamodel.
   transform :: (String -> String) -> String -> String -> [(String, String)] -> (P_Population,P_Declaration)
   transform _ "name" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "naam" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "naam" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) [] [] [] [] OriginUnknown False )
   transform _ "docu" typeLabel tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "documentatie" (Just (P_Sign (PCpt typeLabel) (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "documentatie" (P_Sign (PCpt typeLabel) (PCpt "Tekst")) [] [] [] [] OriginUnknown False )
   transform _ "key" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "key" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "key" (P_Sign (PCpt "Property") (PCpt "Tekst")) [] [] [] [] OriginUnknown False )
   transform _ "value" "Property" tuples
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown "value" (Just (P_Sign (PCpt "Property") (PCpt "Tekst")))) (transTuples tuples)
      , P_Sgn "value" (P_Sign (PCpt "Property") (PCpt "Tekst")) [] [] [] [] OriginUnknown False )
   transform typeLookup "relationship" relLabel tuples@((x,y):_)
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown (unfixRel relLabel) (Just (P_Sign (PCpt (typeLookup x)) (PCpt (typeLookup y))))) (transTuples tuples)
      , P_Sgn "relationship" (P_Sign (PCpt (typeLookup x)) (PCpt (typeLookup y))) [] [] [] [] OriginUnknown False )
      where
       -- transform for example  "archimate:AggregationRelationship"  into  "aggregation"
       unfixRel cs = (reverse.drop 1.dropWhile (/='R').reverse.relCase) cs
       relCase (c:cs) = toLower c: cs
       relCase "" = error "fatal 325 empty relation identifier."
   transform typeLookup relLabel _ tuples@((x,y):_)
    = ( P_RelPopu Nothing Nothing OriginUnknown (PNamedRel OriginUnknown relLabel (Just (P_Sign (PCpt (typeLookup x)) (PCpt (typeLookup y))))) (transTuples tuples)
      , P_Sgn relLabel (P_Sign (PCpt (typeLookup x)) (PCpt (typeLookup y))) [] [] [] [] OriginUnknown False )
   transform _ _ _ _ = error "fatal 328 non-exhaustive pattern in transform"

   transTuples :: [(String, String)] -> [PAtomPair]
   transTuples tuples = [ PPair OriginUnknown (ScriptString OriginUnknown x) (ScriptString OriginUnknown y) | (x,y)<-tuples ]


-- The function `processStraight` derives an ArchiRepo from an Archi-XML-file.
   processStraight :: String -> IOSLA (XIOState s0) XmlTree ArchiRepo
   processStraight infile
    = readDocument [ withRemoveWS  yes        -- purge redundant white spaces
                   , withCheckNamespaces yes  -- propagates name spaces into QNames
                   , withTrace 0]             -- if >0 gives trace messages.
                   infile
      >>>
      analArchiRepo
       where
        analArchiRepo :: ArrowXml a => a XmlTree ArchiRepo
        analArchiRepo
          = atTag "archimate:ArchimateModel" >>>
            proc l -> do repoNm'   <- getAttrValue "name"               -< l
                         repoId'   <- getAttrValue "id"                 -< l
                         folders'  <- listA (getChildren >>> getFolder) -< l
                         props'    <- listA (getChildren >>> getProp)   -< l
                         returnA   -< ArchiRepo { archRepoName   = repoNm'
                                                , archRepoId     = repoId'
                                                , archFolders    = folders'
                                                , archProperties = [ prop{archPropId="pr-"++show i} | (prop,i)<- zip props' [length (allProps folders')..] ]
                                                }

        getFolder :: ArrowXml a => a XmlTree Folder
        getFolder
         = isElem >>> hasName "folders" >>>
            proc l -> do fldNm'     <- getAttrValue "name"                 -< l
                         fldId'     <- getAttrValue "id"                   -< l
                         fldType'   <- getAttrValue "type"                 -< l
                         elems'     <- listA (getChildren >>> getElement)  -< l
                         subFlds'   <- listA (getChildren >>> getFolder)   -< l
                         returnA    -< Folder { fldName    = fldNm'
                                              , fldId      = fldId'
                                              , fldType    = fldType'
                                              , fldElems   = elems'
                                              , fldFolders = subFlds'
                                              }

        getProp :: ArrowXml a => a XmlTree ArchiProp
        getProp = isElem >>> hasName "properties" >>>
            proc l -> do propKey    <- getAttrValue "key"   -< l
                         propVal    <- getAttrValue "value" -< l
                         returnA    -< ArchiProp { archPropKey = propKey
                                                 , archPropId  = error "fatal 315: archPropId not yet defined"
                                                 , archPropVal = propVal
                                                 }

        getElement :: ArrowXml a => a XmlTree Element
        getElement = atTag "elements" >>>
            proc l -> do elemType'  <- getAttrValue "xsi:type"           -< l
                         elemId'    <- getAttrValue "id"                 -< l
                         elemName'  <- getAttrValue "name"               -< l
                         elemSrc'   <- getAttrValue "source"             -< l
                         elemTgt'   <- getAttrValue "target"             -< l
                         elemDocu'  <- getAttrValue "documentation"      -< l
                         childs'    <- listA (getChildren >>> getChild)  -< l
                         props'     <- listA (getChildren >>> getProp)   -< l
                         returnA    -< Element  { elemType = drop 1 (dropWhile (/=':') elemType')  -- drop the prefix "archimate:"
                                               , elemId   = elemId'
                                               , elemName = elemName'
                                               , elemSrc  = elemSrc'
                                               , elemTgt  = elemTgt'
                                               , elemDocu = elemDocu'
                                               , elChilds = childs'
                                               , elProps  = props'
                                               }
                                  
        getRelation :: ArrowXml a => a XmlTree Relation
        getRelation = isElem >>> hasName "relationship" >>>
            proc l -> do relType'   <- getAttrValue "xsi:type"          -< l
                         relHref'   <- getAttrValue "href"              -< l
                         returnA    -< Relation{ relType = relType'
                                               , relHref = relHref'
                                               }

        getBound :: ArrowXml a => a XmlTree Bound
        getBound = isElem >>> hasName "bounds" >>>
            proc l -> do bnd_x'     <- getAttrValue "x"                 -< l
                         bnd_y'     <- getAttrValue "y"                 -< l
                         bndWidth'  <- getAttrValue "width"             -< l
                         bndHeight' <- getAttrValue "height"            -< l
                         returnA    -< Bound   { bnd_x      = bnd_x'
                                               , bnd_y      = bnd_y'
                                               , bnd_width  = bndWidth'
                                               , bnd_height = bndHeight'
                                               }

        getSrcConn :: ArrowXml a => a XmlTree SourceConnection
        getSrcConn = isElem >>> hasName "sourceConnections" >>>
            proc l -> do sConType'  <- getAttrValue "xsi:type"          -< l
                         sConId'    <- getAttrValue "id"                -< l
                         sConSrc'   <- getAttrValue "source"            -< l
                         sConTgt'   <- getAttrValue "target"            -< l
                         sConRel'   <- getAttrValue "relationship"      -< l
                         sConRelat' <- listA (getChildren>>>getRelation)-< l
                         bendPts'   <- listA (getChildren>>>getBendPt)  -< l
                         returnA    -< SrcConn { sConType  = sConType'
                                               , sConId    = sConId'
                                               , sConSrc   = sConSrc'
                                               , sConTgt   = sConTgt'
                                               , sConRel   = sConRel'
                                               , sConRelat = sConRelat'
                                               , sCbendPts = bendPts'
                                               }

        getBendPt :: ArrowXml a => a XmlTree BendPoint
        getBendPt = isElem >>> hasName "bendpoints" >>>
            proc l -> do bpStartX'  <- getAttrValue "startX"              -< l
                         bpStartY'  <- getAttrValue "startY"              -< l
                         bpEndX'    <- getAttrValue "endX"                -< l
                         bpEndY'    <- getAttrValue "endY"                -< l
                         returnA    -< BendPt  { bpStartX  = bpStartX'
                                               , bpStartY  = bpStartY'
                                               , bpEndX    = bpEndX'  
                                               , bpEndY    = bpEndY'  
                                               }
                                    
        getChild                    
         = atTag "children" >>>     
            proc l -> do chldType'  <- getAttrValue "xsi:type"            -< l
                         chldId'    <- getAttrValue "id"                  -< l
--                         chldName'  <- getAttrValue "name"                -< l -- defined, but not used.
                         chldFCol'  <- getAttrValue "fillColor"           -< l
                         chldAlgn'  <- getAttrValue "textAlignment"       -< l
                         chldElem'  <- getAttrValue "archimateElement"    -< l
                         trgtConn'  <- getAttrValue "targetConnections"   -< l
                         bound'     <- getChildren >>> getBound           -< l
                         srcConns'  <- listA (getChildren >>> getSrcConn) -< l
                         childs'    <- listA (getChildren >>> getChild)   -< l
                         returnA    -< Child { chldType = chldType'
                                             , chldId   = chldId'
                                             , chldAlgn = chldAlgn'
                                             , chldFCol = chldFCol'
                                             , chldElem = chldElem'
                                             , trgtConn = trgtConn'
                                             , bound    = bound'
                                             , srcConns = srcConns'
                                             , childs   = childs'
                                             }

-- Auxiliaries

   atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
   atTag tag = deep (isElem >>> hasName tag)