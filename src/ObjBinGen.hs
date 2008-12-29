{-# LINE 1 "ObjBinGen.lhs" #-}
#line 1 "ObjBinGen.lhs"
  module ObjBinGen where
   import Directory
   import Auxiliaries
   import ADLdef
   import CommonClasses
   import ERmodel
   import ObjBinGenLocalsettings
   import ObjBinGenConnectToDataBase
   import ObjBinGenObject
   import ObjBinGenObjectWrapper

   phpObjServices contexts
                  contextname
                  filename
                  dbName
                  targetDir
                  servGen
     =   putStr ("\n---------------------------\nGenerating php Object files with ADL version "++adlVersion++"\n---------------------------")
      >> putStr ("\n  Generating localsettings.inc.php")
      >> do { d <- doesDirectoryExist targetDir
            ; if d
              then putStr ""
              else createDirectory (targetDir) }
      >> writeFile (targetDir++"localsettings.inc.php") ls
      >> putStr ("\n  Generating connectToDataBase.inc.php")
      >> writeFile (targetDir++"connectToDataBase.inc.php") ctdb
      >> putStr ("\nIncludable files for all objects:")
      >> sequence_
         [ putStr ("\n  Generating "++name o++".inc.php")
           >> writeFile (targetDir++name o++".inc.php") (ojs o)
         | o <- serviceObjects
         ]
      >> putStr ("\nWrapper files for all objects:")
      >> sequence_
         [ putStr ("\n  Generating "++name o++".php")
           >> writeFile (targetDir++name o++".php") (wrapper o)
         | o <- serviceObjects
         ]
      >> putStr ("\n\n")
      where
       context = if null ctxs then error ("!Mistake: "++contextname++" not encountered in input file.\n") else head ctxs
       ctxs    = [c| c<-contexts, name c==contextname]
       (datasets, servicesGenerated, rels, ruls) = erAnalysis context
       ls   = localsettings context serviceObjects dbName
       ctdb = connectToDataBase context dbName
       wrapper o = objectWrapper (name o)
       ojs o = objectServices context filename o
       serviceObjects = if servGen then servicesGenerated else attributes context
