{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.SSWC where

import Text.XmlHtml
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Generics hiding (Generic)

deriving instance Data Document
deriving instance Typeable Document
deriving instance Data DocType
deriving instance Typeable DocType
deriving instance Data ExternalID
deriving instance Typeable ExternalID
deriving instance Data Node
deriving instance Typeable Node
deriving instance Data InternalSubset
deriving instance Typeable InternalSubset
deriving instance Data Encoding
deriving instance Typeable Encoding

loadFile :: String -> IO ()
loadFile fnm = do
  edoc <- parseHTML fnm <$> BS.readFile fnm 
  case edoc of 
    Right doc -> do print doc
                    putStrLn "success"
    Left err -> putStrLn $ "error: "++err
