{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Text.Pandoc.Class.PandocMonad
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

This module defines a type class, 'PandocMonad', for pandoc readers
and writers.
-}

module Text.Pandoc.Class.PandocMonad
  ( PandocMonad(..)
  , readFileFromDirs
  , report
  , setTrace
  , setRequestHeader
  , setNoCheckCertificate
  , getLog
  , setVerbosity
  , getVerbosity
  , getMediaBag
  , setMediaBag
  , insertMedia
  , setUserDataDir
  , getUserDataDir
  , fetchItem
  , fetchMediaResource
  , getInputFiles
  , setInputFiles
  , getOutputFile
  , setOutputFile
  , setResourcePath
  , getResourcePath
  , readDefaultDataFile
  , readDataFile
  , fillMediaBag
  , makeCanonical
  ) where

import Control.Monad.Except (MonadError (catchError, throwError),
                             MonadTrans, lift, when)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Maybe (fromMaybe)
import Network.URI ( escapeURIString, nonStrictRelativeTo,
                     unEscapeString, parseURIReference, isAllowedInURI,
                     parseURI, URI(..) )
import System.FilePath ((</>), (<.>), takeExtension, dropExtension,
                        isRelative, splitDirectories)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, getMimeType, extensionFromMimeType)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia)
import Text.Pandoc.Shared (uriPathToPath)
import Text.Pandoc.Walk (walkM)
import Text.Parsec (ParsecT, getPosition, sourceLine, sourceName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Debug.Trace
import qualified System.FilePath.Posix as Posix
import qualified Text.Pandoc.MediaBag as MB
import qualified Text.Pandoc.UTF8 as UTF8
#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#endif

-- | The PandocMonad typeclass contains all the potentially
-- IO-related functions used in pandoc's readers and writers.
-- Instances of this typeclass may implement these functions
-- in IO (as in 'PandocIO') or using an internal state that
-- represents a file system, time, and so on (as in 'PandocPure').
class (Functor m, Applicative m, Monad m, MonadError PandocError m)
      => PandocMonad m where
  -- | Lookup an environment variable.
  lookupEnv :: T.Text -> m (Maybe T.Text)
  -- | Return a new unique integer.
  newUniqueHash :: m Int
  -- | Retrieve contents and mime type from a URL, raising
  -- an error on failure.
  openURL :: T.Text -> m (B.ByteString, Maybe MimeType)
  -- | Read the lazy ByteString contents from a file path,
  -- raising an error on failure.
  readFileLazy :: FilePath -> m BL.ByteString
  -- | Read the strict ByteString contents from a file path,
  -- raising an error on failure.
  readFileStrict :: FilePath -> m B.ByteString
  -- | Return a list of paths that match a glob, relative to
  -- the working directory.  See 'System.FilePath.Glob' for
  -- the glob syntax.
  glob :: String -> m [FilePath]
  -- | Returns True if file exists.
  fileExists :: FilePath -> m Bool
  -- | Returns the path of data file.
  getDataFileName :: FilePath -> m FilePath
  -- | Get the value of the 'CommonState' used by all instances
  -- of 'PandocMonad'.
  getCommonState :: m CommonState
  -- | Set the value of the 'CommonState' used by all instances
  -- of 'PandocMonad'.
  -- | Get the value of a specific field of 'CommonState'.
  putCommonState :: CommonState -> m ()
  -- | Get the value of a specific field of 'CommonState'.
  getsCommonState :: (CommonState -> a) -> m a
  getsCommonState f = f <$> getCommonState
  -- | Modify the 'CommonState'.
  modifyCommonState :: (CommonState -> CommonState) -> m ()
  modifyCommonState f = getCommonState >>= putCommonState . f
  -- | Output a log message.
  logOutput :: LogMessage -> m ()
  -- | Output a debug message to sterr, using 'Debug.Trace.trace',
  -- if tracing is enabled.  Note: this writes to stderr even in
  -- pure instances.
  trace :: T.Text -> m ()
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ Debug.Trace.trace ("[trace] " ++ T.unpack msg) (return ())

-- * Functions defined for all PandocMonad instances

-- | Set the verbosity level.
setVerbosity :: PandocMonad m => Verbosity -> m ()
setVerbosity verbosity =
  modifyCommonState $ \st -> st{ stVerbosity = verbosity }

-- | Get the verbosity level.
getVerbosity :: PandocMonad m => m Verbosity
getVerbosity = getsCommonState stVerbosity

-- | Get the accomulated log messages (in temporal order).
getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

-- | Log a message using 'logOutput'.  Note that 'logOutput' is
-- called only if the verbosity level exceeds the level of the
-- message, but the message is added to the list of log messages
-- that will be retrieved by 'getLog' regardless of its verbosity level.
report :: PandocMonad m => LogMessage -> m ()
report msg = do
  verbosity <- getsCommonState stVerbosity
  let level = messageVerbosity msg
  when (level <= verbosity) $ logOutput msg
  modifyCommonState $ \st -> st{ stLog = msg : stLog st }

-- | Determine whether tracing is enabled.  This affects
-- the behavior of 'trace'.  If tracing is not enabled,
-- 'trace' does nothing.
setTrace :: PandocMonad m => Bool -> m ()
setTrace useTracing = modifyCommonState $ \st -> st{stTrace = useTracing}

-- | Set request header to use in HTTP requests.
setRequestHeader :: PandocMonad m
                 => T.Text  -- ^ Header name
                 -> T.Text  -- ^ Value
                 -> m ()
setRequestHeader name val = modifyCommonState $ \st ->
  st{ stRequestHeaders =
       (name, val) : filter (\(n,_) -> n /= name) (stRequestHeaders st)  }

-- | Determine whether certificate validation is disabled
setNoCheckCertificate :: PandocMonad m => Bool -> m ()
setNoCheckCertificate noCheckCertificate = modifyCommonState $ \st -> st{stNoCheckCertificate = noCheckCertificate}

-- | Initialize the media bag.
setMediaBag :: PandocMonad m => MediaBag -> m ()
setMediaBag mb = modifyCommonState $ \st -> st{stMediaBag = mb}

-- | Retrieve the media bag.
getMediaBag :: PandocMonad m => m MediaBag
getMediaBag = getsCommonState stMediaBag

-- | Insert an item into the media bag.
insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp mime bs = do
  mb <- getMediaBag
  let mb' = MB.insertMedia fp mime bs mb
  setMediaBag mb'

-- | Retrieve the input filenames.
getInputFiles :: PandocMonad m => m [FilePath]
getInputFiles = getsCommonState stInputFiles

-- | Set the input filenames.
setInputFiles :: PandocMonad m => [FilePath] -> m ()
setInputFiles fs = do
  let sourceURL = case fs of
                    []    -> Nothing
                    (x:_) -> case parseURI x of
                                Just u
                                  | uriScheme u `elem` ["http:","https:"] ->
                                      Just $ show u{ uriQuery = "",
                                                     uriFragment = "" }
                                _ -> Nothing

  modifyCommonState $ \st -> st{ stInputFiles = fs
                               , stSourceURL = T.pack <$> sourceURL }

-- | Retrieve the output filename.
getOutputFile :: PandocMonad m => m (Maybe FilePath)
getOutputFile = getsCommonState stOutputFile

-- | Set the output filename.
setOutputFile :: PandocMonad m => Maybe FilePath -> m ()
setOutputFile mbf = modifyCommonState $ \st -> st{ stOutputFile = mbf }

-- | Retrieve the resource path searched by 'fetchItem'.
getResourcePath :: PandocMonad m => m [FilePath]
getResourcePath = getsCommonState stResourcePath

-- | Set the resource path searched by 'fetchItem'.
setResourcePath :: PandocMonad m => [FilePath] -> m ()
setResourcePath ps = modifyCommonState $ \st -> st{stResourcePath = ps}

-- | Read file, checking in any number of directories.
readFileFromDirs :: PandocMonad m => [FilePath] -> FilePath -> m (Maybe T.Text)
readFileFromDirs [] _ = return Nothing
readFileFromDirs (d:ds) f = catchError
    (Just . T.pack . UTF8.toStringLazy <$> readFileLazy (d </> f))
    (\_ -> readFileFromDirs ds f)

-- | Specialized version of parseURIReference that disallows
-- single-letter schemes.  Reason:  these are usually windows absolute
-- paths.
parseURIReference' :: T.Text -> Maybe URI
parseURIReference' s = do
  u <- parseURIReference (T.unpack s)
  case uriScheme u of
       [_] -> Nothing
       _   -> Just u

-- | Set the user data directory in common state.
setUserDataDir :: PandocMonad m
               => Maybe FilePath
               -> m ()
setUserDataDir mbfp = modifyCommonState $ \st -> st{ stUserDataDir = mbfp }

-- | Get the user data directory from common state.
getUserDataDir :: PandocMonad m
               => m (Maybe FilePath)
getUserDataDir = getsCommonState stUserDataDir

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: PandocMonad m
          => T.Text
          -> m (B.ByteString, Maybe MimeType)
fetchItem s = do
  mediabag <- getMediaBag
  case lookupMedia (T.unpack s) mediabag of
    Just (mime, bs) -> return (BL.toStrict bs, Just mime)
    Nothing -> downloadOrRead s

-- | Returns the content and, if available, the MIME type of a resource.
-- If the given resource location is a valid URI, then download the
-- resource from that URI. Otherwise, treat the resource identifier as a
-- local file name.
--
-- Note that resources are treated relative to the URL of the first
-- input source, if any.
downloadOrRead :: PandocMonad m
               => T.Text
               -> m (B.ByteString, Maybe MimeType)
downloadOrRead s = do
  sourceURL <- getsCommonState stSourceURL
  case (sourceURL >>= parseURIReference' .
                       ensureEscaped, ensureEscaped s) of
    (Just u, s') -> -- try fetching from relative path at source
       case parseURIReference' s' of
            Just u' -> openURL $ T.pack $ show $ u' `nonStrictRelativeTo` u
            Nothing -> openURL s' -- will throw error
    (Nothing, s'@(T.unpack -> ('/':'/':c:_))) | c /= '?' ->  -- protocol-relative URI
                -- we exclude //? because of //?UNC/ on Windows
       case parseURIReference' s' of
            Just u' -> openURL $ T.pack $ show $ u' `nonStrictRelativeTo` httpcolon
            Nothing -> openURL s' -- will throw error
    (Nothing, s') ->
       case parseURI (T.unpack s') of  -- requires absolute URI
            Just u' | uriScheme u' == "file:" ->
                 readLocalFile $ uriPathToPath (T.pack $ uriPath u')
            -- We don't want to treat C:/ as a scheme:
            Just u' | length (uriScheme u') > 2 -> openURL (T.pack $ show u')
            _ -> readLocalFile fp -- get from local file system
   where readLocalFile f = do
             resourcePath <- getResourcePath
             cont <- if isRelative f
                        then withPaths resourcePath readFileStrict f
                        else readFileStrict f
             return (cont, mime)
         httpcolon = URI{ uriScheme = "http:",
                          uriAuthority = Nothing,
                          uriPath = "",
                          uriQuery = "",
                          uriFragment = "" }
         dropFragmentAndQuery = T.takeWhile (\c -> c /= '?' && c /= '#')
         fp = unEscapeString $ T.unpack $ dropFragmentAndQuery s
         mime = getMimeType $ case takeExtension fp of
                     ".gz" -> dropExtension fp
                     ".svgz" -> dropExtension fp ++ ".svg"
                     x     -> x
         ensureEscaped = T.pack . escapeURIString isAllowedInURI . T.unpack . T.map convertSlash
         convertSlash '\\' = '/'
         convertSlash x    = x

-- | Read file from user data directory or,
-- if not found there, from Cabal data directory.
readDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDataFile fname = do
  datadir <- getUserDataDir
  case datadir of
       Nothing -> readDefaultDataFile fname
       Just userDir -> do
         exists <- fileExists (userDir </> fname)
         if exists
            then readFileStrict (userDir </> fname)
            else readDefaultDataFile fname

-- | Read file from from Cabal data directory.
readDefaultDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDefaultDataFile fname =
#ifdef EMBED_DATA_FILES
  case lookup (makeCanonical fname) dataFiles of
    Nothing       -> throwError $ PandocCouldNotFindDataFileError $ T.pack fname
    Just contents -> return contents
#else
  getDataFileName fname' >>= checkExistence >>= readFileStrict
    where fname' = if fname == "MANUAL.txt" then fname else "data" </> fname

-- | Returns the input filename unchanged if the file exits, and throws
-- a `PandocCouldNotFindDataFileError` if it doesn't.
checkExistence :: PandocMonad m => FilePath -> m FilePath
checkExistence fn = do
  exists <- fileExists fn
  if exists
     then return fn
     else throwError $ PandocCouldNotFindDataFileError $ T.pack fn
#endif

-- | Canonicalizes a file path by removing redundant @.@ and @..@.
makeCanonical :: FilePath -> FilePath
makeCanonical = Posix.joinPath . transformPathParts . splitDirectories
 where  transformPathParts = reverse . foldl go []
        go as     "."  = as
        go (_:as) ".." = as
        go as     x    = x : as

-- | Trys to run an action on a file: for each directory given, a
-- filepath is created from the given filename, and the action is run on
-- that filepath. Returns the result of the first successful execution
-- of the action, or throws a @PandocResourceNotFound@ exception if the
-- action errors for all filepaths.
withPaths :: PandocMonad m => [FilePath] -> (FilePath -> m a) -> FilePath -> m a
withPaths [] _ fp = throwError $ PandocResourceNotFound $ T.pack fp
withPaths (p:ps) action fp =
  catchError (action (p </> fp))
             (\_ -> withPaths ps action fp)

-- | Fetch local or remote resource (like an image) and provide data suitable
-- for adding it to the MediaBag.
fetchMediaResource :: PandocMonad m
              => T.Text -> m (FilePath, Maybe MimeType, BL.ByteString)
fetchMediaResource src = do
  (bs, mt) <- downloadOrRead src
  let ext = fromMaybe (T.pack $ takeExtension $ T.unpack src)
                      (mt >>= extensionFromMimeType)
  let bs' = BL.fromChunks [bs]
  let basename = showDigest $ sha1 bs'
  let fname = basename <.> T.unpack ext
  return (fname, mt, bs')

-- | Traverse tree, filling media bag for any images that
-- aren't already in the media bag.
fillMediaBag :: PandocMonad m => Pandoc -> m Pandoc
fillMediaBag d = walkM handleImage d
  where handleImage :: PandocMonad m => Inline -> m Inline
        handleImage (Image attr lab (src, tit)) = catchError
          (do mediabag <- getMediaBag
              case lookupMedia (T.unpack src) mediabag of
                Just (_, _) -> return $ Image attr lab (src, tit)
                Nothing -> do
                  (fname, mt, bs) <- fetchMediaResource src
                  insertMedia fname mt bs
                  return $ Image attr lab (T.pack fname, tit))
          (\e ->
              case e of
                PandocResourceNotFound _ -> do
                  report $ CouldNotFetchResource src
                            "replacing image with description"
                  -- emit alt text
                  return $ Span ("",["image"],[]) lab
                _ -> throwError e)
        handleImage x = return x

-- This requires UndecidableInstances.  We could avoid that
-- by repeating the definitions below for every monad transformer
-- we use: ReaderT, WriterT, StateT, RWST.  But this seems to
-- be harmless.
instance (MonadTrans t, PandocMonad m, Functor (t m),
          MonadError PandocError (t m), Monad (t m),
          Applicative (t m)) => PandocMonad (t m) where
  lookupEnv = lift . lookupEnv
  newUniqueHash = lift newUniqueHash
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  glob = lift . glob
  fileExists = lift . fileExists
  getDataFileName = lift . getDataFileName
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState
  logOutput = lift . logOutput

instance {-# OVERLAPS #-} PandocMonad m => PandocMonad (ParsecT s st m) where
  lookupEnv = lift . lookupEnv
  newUniqueHash = lift newUniqueHash
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  glob = lift . glob
  fileExists = lift . fileExists
  getDataFileName = lift . getDataFileName
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ do
      pos <- getPosition
      Debug.Trace.trace
        ("[trace] Parsed " ++ T.unpack msg ++ " at line " ++
            show (sourceLine pos) ++
            if sourceName pos == "chunk"
               then " of chunk"
               else "")
        (return ())
  logOutput = lift . logOutput
