------------------------------------------------------------------------------
-- | This library defines various I/O actions to read Curry programs and
--   transform them into the AbstractCurry representation and to write
--   AbstractCurry files.
--
--   Assumption: an abstract Curry program is stored in file with
--   extension `.acy` in the subdirectory `.curry`
--
--   Author : Michael Hanus, Bjoern Peemoeller, Jan Tikovsky, Finn Teegen
--   Version: September 2025
-------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module AbstractCurry.Files
  ( readCurry, readCurryWithParseOptions, readCurryWithImports, tryReadCurryFile
  , readUntypedCurry, readUntypedCurryWithParseOptions
  , abstractCurryFileName, untypedAbstractCurryFileName
  , readAbstractCurryFile, tryReadACYFile
  , writeAbstractCurryFile )
 where

import Data.Char            ( isSpace )
import System.IO            ( IOMode(..), hGetContents, openFile )

import System.Directory     ( doesFileExist, getModificationTime
                            , findFileWithSuffix, getFileWithSuffix )
import System.FilePath      ( takeFileName, (</>), (<.>) )
import System.CurryPath     ( getLoadPathForModule, inCurrySubdir
                            , lookupModuleSourceInLoadPath, stripCurrySuffix )
import System.FrontendExec
import ReadShowTerm         ( readsUnqualifiedTerm, showTerm )

import AbstractCurry.Select ( imports )
import AbstractCurry.Types

------------------------------------------------------------------------------
-- | I/O action which parses a Curry program and returns the corresponding
--   typed Abstract Curry program.
--   Thus, the argument is the file name without suffix ".curry"
--   or ".lcurry") and the result is a Curry term representing this
--   program.
readCurry :: String -> IO CurryProg
readCurry prog = readCurryWithParseOptions prog (setQuiet True defaultParams)

-- |  Read an AbstractCurry file with all its imports.
readCurryWithImports
  :: String         -- ^ Module name or file name of Curry module
  -> IO [CurryProg] -- ^ List of curry programs, having
                    --   the `AbstractCurry` file as head
readCurryWithImports modname = collect [] [modname]
 where
  collect _        []     = return []
  collect imported (m:ms)
    | m `elem` imported   = collect imported ms
    | otherwise           = do
      p <- readCurry m
      ps <- collect (m:imported) (ms ++ imports p)
      return (p:ps)

tryReadCurryWithImports :: String -> IO (Either [String] [CurryProg])
tryReadCurryWithImports modname = collect [] [modname]
 where
  collect _        []     = return (Right [])
  collect imported (m:ms)
    | m `elem` imported   = collect imported ms
    | otherwise           = do
      eProg <- tryReadCurryFile m
      case eProg of
        Left err                          -> return (Left [err])
        Right prog@(CurryProg _ is _ _ _ _ _ _) -> do
          results <- collect (m:imported) (ms ++ is)
          return (either Left (Right . (prog :)) results)

-- | I/O action which tries to parse a Curry module and returns
--   either an error message or the corresponding AbstractCurry program.
tryReadCurryFile :: String -> IO (Either String CurryProg)
tryReadCurryFile m = do
  mbSrc <- lookupModuleSourceInLoadPath m
  case mbSrc of
    Nothing      -> cancel $ "Source module '" ++ m ++ "' not found"
    Just (_,srcFile) -> do
      callFrontendWithParams ACY (setQuiet True defaultParams) m
      mbFn <- getLoadPathForModule m >>=
              findFileWithSuffix (abstractCurryFileName m) [""]
      case mbFn of
        Nothing -> cancel $ "AbstractCurry module '" ++ m ++ "' not found"
        Just fn -> do
          ctime <- getModificationTime srcFile
          ftime <- getModificationTime fn
          if ctime > ftime
            then cancel $ "Source file '" ++ srcFile
                    ++ "' is newer than AbstractCurry file '" ++ fn ++ "'"
            else do
              mbProg <- tryParse fn
              case mbProg of
                Left  err -> cancel err
                Right p   -> return (Right p)
 where cancel str = return (Left str)

-- | Try to parse an AbstractCurry file.
tryParse
  :: String -- ^ File name of AbstractCurry file
  -> IO (Either String CurryProg)
tryParse fn = do
  exists <- doesFileExist fn
  if not exists
    then cancel $ "AbstractCurry file '" ++ fn ++ "' does not exist"
    else do
      src <- readCompleteFile fn
      let (line1, lines) = break (=='\n') src
      if line1 /= "{- "++version++" -}"
        then cancel $ "Could not parse AbstractCurry file '" ++ fn
                   ++ "': incompatible versions"
        else
          case readACYString lines of
            Just p  -> return (Right p)
            Nothing -> cancel $ "Could not parse AbstractCurry file '" ++
                                fn ++ "': no parse"
 where cancel str = return (Left str)

-- | I/O action which parses a Curry program and returns the corresponding
--   untyped AbstractCurry program.
--   The argument is the file name without suffix ".curry"
--   or ".lcurry") and the result is a Curry term representing this
--   program.
--   In an untyped AbstractCurry program, the type signatures
--   of operations are the type signatures provided by the programmer
--   (and not the type signatures inferred by the front end).
--   If the programmer has not provided an explicit type signature,
--   the function declaration contains the type `(CTCons ("Prelude","untyped")`.
readUntypedCurry :: String -> IO CurryProg
readUntypedCurry prog =
  readUntypedCurryWithParseOptions prog (setQuiet True defaultParams)

-- | I/O action which reads a typed Curry program from a file (with extension
--   ".acy") with respect to some parser options.
--   This I/O action is used by the standard action 'readCurry'.
--   It is currently predefined only in Curry2Prolog.
readCurryWithParseOptions
  :: String         -- ^ The program file name (without suffix ".curry")
  -> FrontendParams -- ^ parameters passed to the front end
  -> IO CurryProg
readCurryWithParseOptions progname options = do
  let modname = takeFileName progname
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find AbstractCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix (abstractCurryFileName modname) [""] loadpath
      readAbstractCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams ACY options progname
      readAbstractCurryFile (abstractCurryFileName (dir </> modname))

-- | I/O action which reads an untyped Curry program from a file (with extension
--   ".uacy") with respect to some parser options. For more details
--   see function 'readCurryWithParseOptions'
--   In an untyped AbstractCurry program, the type signatures
--   of operations are the type signatures provided by the programmer
--   (and not the type signatures inferred by the front end).
--   If the programmer has not provided an explicit type signature,
--   the function declaration contains the type `(CTCons ("Prelude","untyped")`.
readUntypedCurryWithParseOptions :: String -> FrontendParams -> IO CurryProg
readUntypedCurryWithParseOptions progname options = do
  let modname = takeFileName progname
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find AbstractCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix (untypedAbstractCurryFileName modname) [""]
                                loadpath
      readAbstractCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams UACY options progname
      readAbstractCurryFile (untypedAbstractCurryFileName (dir </> modname))

-- | Transforms a name of a Curry program (with or without suffix ".curry"
--   or ".lcurry") into the name of the file containing the
--   corresponding AbstractCurry program.
abstractCurryFileName :: String -> String
abstractCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "acy"

-- | Transforms a name of a Curry program (with or without suffix ".curry"
--   or ".lcurry") into the name of the file containing the
--   corresponding untyped AbstractCurry program.
untypedAbstractCurryFileName :: String -> String
untypedAbstractCurryFileName prog =
  inCurrySubdir (stripCurrySuffix prog) <.> "uacy"

-- | I/O action which reads an AbstractCurry program from a file in ".acy"
--   format. In contrast to <CODE>readCurry</CODE>, this action does not parse
--   a source program. Thus, the argument must be the name of an existing
--   file (with suffix ".acy") containing an AbstractCurry program in ".acy"
--   format and the result is a Curry term representing this program.
--   It is currently predefined only in Curry2Prolog.
readAbstractCurryFile :: FilePath -> IO CurryProg
readAbstractCurryFile filename = do
  exacy <- doesFileExist filename
  if exacy
   then readExistingACY filename
   else do let subdirfilename = inCurrySubdir filename
           exdiracy <- doesFileExist subdirfilename
           if exdiracy
            then readExistingACY subdirfilename
            else error ("EXISTENCE ERROR: AbstractCurry file '"++filename++
                        "' does not exist")
 where
   readExistingACY fname = do
     filecontents <- readCompleteFile fname
     let (line1,lines) = break (=='\n') filecontents
     if line1 == "{- "++version++" -}"
      then case readACYString lines of
             Just p  -> return p
             Nothing -> error $ "Could not parse AbstractCurry file '" ++
                                filename ++ "': no parse"
      else error $ "AbstractCurry: incompatible file found: "++fname

-- | Tries to read an AbstractCurry file and returns 'Nothing' if the file
--   does not exist or 'Just prog' if the file exists and contains
--   a valid AbstractCurry program.
tryReadACYFile :: String -> IO (Maybe CurryProg)
tryReadACYFile fn = do
  exists <- doesFileExist fn
  if exists
    then tryRead fn
    else do
      let fn' = inCurrySubdir fn
      exists' <- doesFileExist fn'
      if exists'
        then tryRead fn'
        else return Nothing
 where
  tryRead file = do
    src <- readCompleteFile file
    let (line1,lines) = break (=='\n') src
    if line1 /= "{- "++version++" -}"
      then error $ "AbstractCurry: incompatible file found: "++fn
      else return $ readACYString lines

readACYString :: String -> Maybe CurryProg
readACYString s =
  case
#ifdef __KMCC__
       reads s
#else
       readsUnqualifiedTerm ["AbstractCurry.Types","Prelude"] s
#endif
    of []       -> Nothing
       [(p,tl)] -> if all isSpace tl then Just p
                                     else Nothing
       _        -> Nothing

-- | Writes an AbstractCurry program into a file in ".acy" format.
--   The first argument must be the name of the target file
--   (with suffix ".acy").
writeAbstractCurryFile :: String -> CurryProg -> IO ()
writeAbstractCurryFile file prog =
#ifdef __KMCC__
  writeFile file (show prog)
#else
  writeFile file (showTerm prog)
#endif

readCompleteFile :: FilePath -> IO String
readCompleteFile fn = openFile fn ReadMode >>= hGetContents

------------------------------------------------------------------------------
