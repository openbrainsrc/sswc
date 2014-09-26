{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

module Text.SSWC 
    ( loadDocument
    , renderDocument
    , getTemplates
    , runTemplates
    , Document(..)) where

import Text.XmlHtml
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Generics hiding (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
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

loadDocument :: String -> IO (Either String Document)
loadDocument fnm = do
  parseHTML fnm <$> BS.readFile fnm 

renderDocument :: Document -> T.Text
renderDocument =  DTE.decodeUtf8  . Builder.toByteString . render

type Templates = M.Map T.Text [Node] 

getTemplates :: Document -> Templates
getTemplates = M.fromList . qquery f where
  f (Element "template" attrs childs) 
      = case lookup "name" attrs of
          Just nm -> [(nm,chompTextNodes childs)]
          Nothing -> []
  f _ = []

runTemplates :: Templates -> Document -> Document
runTemplates ts d = d { docContent = traverse $ docContent d } where
  traverse :: [Node] -> [Node]
  traverse = map onChildren . subTemplates . filter (not . isTemplate) 
  isTemplate (Element "template" _ _) = True
  isTemplate _ = False
  subTemplates [] = []
  subTemplates (e@(Element tagnm _ _):es) =
      case M.lookup tagnm ts of
        Just elms -> elms ++ subTemplates es
        Nothing -> e : subTemplates es
  subTemplates (e:es) = e : subTemplates es
  onChildren node@(Element _ _ _) = node { elementChildren = traverse $ elementChildren node}
  onChildren n = n

qquery :: (Data a1, Typeable b) => (b -> [a]) -> a1 -> [a]
qquery qf = everything (++) ([] `mkQ` qf)

chompTextNodes :: [Node] -> [Node]
chompTextNodes = reverse . dropWhile chompMe . reverse . dropWhile chompMe where
  chompMe (TextNode s) = T.all chompc s
  chompMe _ = False

  chompc ' ' = True
  chompc '\n' = True
  chompc '\t' = True
  chompc _ = False
