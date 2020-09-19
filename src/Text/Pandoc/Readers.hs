{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Readers
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the readers.

Note:  all of the readers assume that the input text has @'\n'@
line endings.  So if you get your input text from a web form,
you should remove @'\r'@ characters using @filter (/='\r')@.

-}

module Text.Pandoc.Readers
  (
    -- * Readers: converting /to/ Pandoc format
    Reader (..)
  , readers
  , readMarkdown
  , readLaTeX
  -- * Miscellaneous
  , getReader
  , getDefaultExtensions
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.Markdown
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Parsec.Error

data Reader m = TextReader (ReaderOptions -> Text -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- | Association list of formats and readers.
readers :: PandocMonad m => [(Text, Reader m)]
readers = [ ("json"         , TextReader readJSON)
           ,("markdown"     , TextReader readMarkdown)
           ,("markdown_strict" , TextReader readMarkdown)
           ,("markdown_phpextra" , TextReader readMarkdown)
           ,("markdown_github" , TextReader readMarkdown)
           ,("markdown_mmd",  TextReader readMarkdown)
           ,("latex"        , TextReader readLaTeX)
           ]

-- | Retrieve reader, extensions based on formatSpec (format+extensions).
getReader :: PandocMonad m => Text -> m (Reader m, Extensions)
getReader s =
  case parseFormatSpec s of
       Left e  -> throwError $ PandocAppError
                    $ T.intercalate "\n" [T.pack m | Message m <- errorMessages e]
       Right (readerName, extsToEnable, extsToDisable) ->
           case lookup readerName readers of
                   Nothing  -> throwError $ PandocUnknownReaderError
                                             readerName
                   Just  r  -> do
                     let allExts = getAllExtensions readerName
                     let exts = foldr disableExtension
                           (foldr enableExtension
                             (getDefaultExtensions readerName)
                                   extsToEnable) extsToDisable
                     mapM_ (\ext ->
                              unless (extensionEnabled ext allExts) $
                                throwError $
                                   PandocUnsupportedExtensionError
                                   (T.drop 4 $ T.pack $ show ext) readerName)
                          (extsToEnable ++ extsToDisable)
                     return (r, exts)

-- | Read pandoc document from JSON format.
readJSON :: PandocMonad m
         => ReaderOptions -> Text -> m Pandoc
readJSON _ t =
  case eitherDecode' . BL.fromStrict . UTF8.fromText $ t of
       Right doc -> return doc
       Left e    -> throwError $ PandocParseError ("JSON parse error: " <> T.pack e)
