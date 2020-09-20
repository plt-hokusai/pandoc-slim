{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Class.IO
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

Default ways to perform @'PandocMonad'@ actions in a @'MonadIO'@ type.

These functions are used to make the @'PandocIO'@ type an instance of
@'PandocMonad'@, but can be reused for any other MonadIO-conforming
types.
-}
module Text.Pandoc.Class.IO
  ( fileExists
  , getDataFileName
  , glob
  , logOutput
  , logIOError
  , lookupEnv
  , newUniqueHash
  , openURL
  , readFileLazy
  , readFileStrict
  , extractMedia
 ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Base64 (decodeLenient)
import Data.Text (Text, pack, unpack)
import Data.Unique (hashUnique)
import Network.URI (unEscapeString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, normalise)
import System.IO (stderr)
import System.IO.Error
import Text.Pandoc.Class.PandocMonad (PandocMonad, getMediaBag, report)
import Text.Pandoc.Definition (Pandoc, Inline (Image))
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging (LogMessage (..), messageVerbosity, showLogMessage)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Text.Pandoc.Walk (walk)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Unique
import qualified System.Directory
import qualified System.Environment as Env
import qualified System.FilePath.Glob
import qualified Text.Pandoc.UTF8 as UTF8

-- | Utility function to lift IO errors into 'PandocError's.
liftIOError :: (PandocMonad m, MonadIO m) => (String -> IO a) -> String -> m a
liftIOError f u = do
  res <- liftIO $ tryIOError $ f u
  case res of
         Left e  -> throwError $ PandocIOError (pack u) e
         Right r -> return r

-- | Show potential IO errors to the user continuing execution anyway
logIOError :: (PandocMonad m, MonadIO m) => IO () -> m ()
logIOError f = do
  res <- liftIO $ tryIOError f
  case res of
    Left e -> report $ IgnoredIOError $ pack $ E.displayException e
    Right _ -> pure ()

-- | Lookup an environment variable in the programs environment.
lookupEnv :: MonadIO m => Text -> m (Maybe Text)
lookupEnv = fmap (fmap pack) . liftIO . Env.lookupEnv . unpack

-- | Return a new unique integer.
newUniqueHash :: MonadIO m => m Int
newUniqueHash = hashUnique <$> liftIO Data.Unique.newUnique

openURL :: (PandocMonad m, MonadIO m) => Text -> m (B.ByteString, Maybe MimeType)
openURL u
 | Just u'' <- T.stripPrefix "data:" u = do
     let mime     = T.takeWhile (/=',') u''
     let contents = UTF8.fromString $
                     unEscapeString $ T.unpack $ T.drop 1 $ T.dropWhile (/=',') u''
     return (decodeLenient contents, Just mime)
 | otherwise = throwError (PandocShouldNeverHappenError "non-data: openURL is not supported in pandoc-slim")

-- | Read the lazy ByteString contents from a file path, raising an error on
-- failure.
readFileLazy :: (PandocMonad m, MonadIO m) => FilePath -> m BL.ByteString
readFileLazy s = liftIOError BL.readFile s

-- | Read the strict ByteString contents from a file path,
-- raising an error on failure.
readFileStrict :: (PandocMonad m, MonadIO m) => FilePath -> m B.ByteString
readFileStrict s = liftIOError B.readFile s

-- | Return a list of paths that match a glob, relative to the working
-- directory. See 'System.FilePath.Glob' for the glob syntax.
glob :: (PandocMonad m, MonadIO m) => String -> m [FilePath]
glob = liftIOError System.FilePath.Glob.glob

-- | Returns True if file exists.
fileExists :: (PandocMonad m, MonadIO m) => FilePath -> m Bool
fileExists = liftIOError System.Directory.doesFileExist

-- | Returns the path of data file.
getDataFileName :: (PandocMonad m, MonadIO m) => FilePath -> m FilePath
getDataFileName = return

-- | Output a log message.
logOutput :: (PandocMonad m, MonadIO m) => LogMessage -> m ()
logOutput msg = liftIO $ do
  UTF8.hPutStr stderr $
      "[" ++ show (messageVerbosity msg) ++ "] "
  alertIndent $ T.lines $ showLogMessage msg

-- | Prints the list of lines to @stderr@, indenting every but the first
-- line by two spaces.
alertIndent :: [Text] -> IO ()
alertIndent [] = return ()
alertIndent (l:ls) = do
  UTF8.hPutStrLn stderr $ unpack l
  mapM_ go ls
  where go l' = do UTF8.hPutStr stderr "  "
                   UTF8.hPutStrLn stderr $ unpack l'

-- | Extract media from the mediabag into a directory.
extractMedia :: (PandocMonad m, MonadIO m) => FilePath -> Pandoc -> m Pandoc
extractMedia dir d = do
  media <- getMediaBag
  case [fp | (fp, _, _) <- mediaDirectory media] of
    []  -> return d
    fps -> do
      mapM_ (writeMedia dir media) fps
      return $ walk (adjustImagePath dir fps) d

-- | Write the contents of a media bag to a path.
writeMedia :: (PandocMonad m, MonadIO m)
           => FilePath -> MediaBag -> FilePath
           -> m ()
writeMedia dir mediabag subpath = do
  -- we join and split to convert a/b/c to a\b\c on Windows;
  -- in zip containers all paths use /
  let fullpath = dir </> unEscapeString (normalise subpath)
  let mbcontents = lookupMedia subpath mediabag
  case mbcontents of
       Nothing -> throwError $ PandocResourceNotFound $ pack subpath
       Just (_, bs) -> do
         report $ Extracting $ pack fullpath
         liftIOError (createDirectoryIfMissing True) (takeDirectory fullpath)
         logIOError $ BL.writeFile fullpath bs

-- | If the given Inline element is an image with a @src@ path equal to
-- one in the list of @paths@, then prepends @dir@ to the image source;
-- returns the element unchanged otherwise.
adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | unpack src `elem` paths = Image attr lab (pack dir <> "/" <> src, tit)
adjustImagePath _ _ x = x
