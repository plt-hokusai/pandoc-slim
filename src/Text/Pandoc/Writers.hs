{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports all writers functions.
-}
module Text.Pandoc.Writers
  (
    -- * Writers: converting /from/ Pandoc format
      Writer(..)
    , writers
    , writeJSON
    , writeHtml4
    , writeHtml4String
    , writeHtml5
    , writeHtml5String
    , getWriter
    ) where

import Control.Monad.Except (throwError)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Error
import Text.Pandoc.Writers.HTML
import Text.Parsec.Error

data Writer m = TextWriter (WriterOptions -> Pandoc -> m Text)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Association list of formats and writers.
writers :: PandocMonad m => [ (Text, Writer m) ]
writers = [ ("html",  TextWriter writeHtml5String)
          , ("html4", TextWriter writeHtml4String)
          , ("html5", TextWriter writeHtml5String)
          , ("json",  TextWriter writeJSON)
          ]

-- | Retrieve writer, extensions based on formatSpec (format+extensions).
getWriter :: PandocMonad m => Text -> m (Writer m, Extensions)
getWriter s =
  case parseFormatSpec s of
        Left e  -> throwError $ PandocAppError
                    $ T.intercalate "\n" [T.pack m | Message m <- errorMessages e]
        Right (writerName, extsToEnable, extsToDisable) ->
           case lookup writerName writers of
                   Nothing  -> throwError $
                                 PandocUnknownWriterError writerName
                   Just  w  -> do
                     let allExts = getAllExtensions writerName
                     let exts = foldr disableExtension
                           (foldr enableExtension
                             (getDefaultExtensions writerName)
                                   extsToEnable) extsToDisable
                     mapM_ (\ext ->
                              unless (extensionEnabled ext allExts) $
                                throwError $
                                   PandocUnsupportedExtensionError
                                   (T.drop 4 $ T.pack $ show ext) writerName)
                          (extsToEnable ++ extsToDisable)
                     return (w, exts)


writeJSON :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJSON _ = return . UTF8.toText . BL.toStrict . encode
