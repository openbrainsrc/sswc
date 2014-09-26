{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

module Text.SSWC where

import Text.XmlHtml
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Generics hiding (Generic)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Blaze.ByteString.Builder as Builder

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
    Right doc -> do let ts = getTemplates doc
                        newDoc = runTemplates ts doc
                        docbs = Builder.toByteString $ render newDoc
                    print ts
                    BS.putStrLn docbs
    Left err -> putStrLn $ "error: "++err

type Templates = M.Map T.Text [Node] 

getTemplates :: Document -> Templates
getTemplates = M.fromList . qquery f where
  f (Element "template" attrs childs) 
      = case lookup "name" attrs of
          Just nm -> [(nm,childs)]
          Nothing -> []
  f _ = []

runTemplates :: Templates -> Document -> Document
runTemplates ts = qmap f where
  f (Element "template" _ _) = Comment "template"
  f e@(Element tagnm _ _) = case M.lookup tagnm ts of
                              Just [elm] -> elm
                              Just elms -> Element "div" [] elms
                              Nothing -> e
  f e = e


qquery :: (Data a1, Typeable b) => (b -> [a]) -> a1 -> [a]
qquery qf = everything (++) ([] `mkQ` qf)

qmap :: (Data a, Typeable b) => (b -> b) -> a -> a
qmap f = everywhere (mkT f)
