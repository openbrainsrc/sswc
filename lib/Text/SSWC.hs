{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

module Text.SSWC 
    ( loadDocument
    , renderDocument
    , loadTemplates
    , loadTemplatesFromFile
    , getTemplates
    , runTemplates
    , template
    , Document(..)) where

import Text.XmlHtml
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Generics hiding (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Map.Strict as M
import qualified Blaze.ByteString.Builder as Builder
import Data.Either
import qualified Text.Blaze.Html as H

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

template :: Templates -> T.Text -> H.Html
template t nm = case M.lookup nm t of
                  Nothing -> error $ "sswc: cannot find template "++T.unpack nm
                  Just ns -> H.unsafeByteString . Builder.toByteString . renderHtmlFragment UTF8 $ ns
                  
loadTemplatesFromFile :: String -> IO (Either String Templates)
loadTemplatesFromFile fnm = do
  edoc <- loadDocument fnm
  case edoc of 
    Left err -> return $ Left err
    Right doc -> loadTemplates doc

loadTemplates :: Document -> IO (Either String Templates)
loadTemplates = lT [] where
 lT prevloads doc = do
   let these = getTemplates doc 
       getLink (Element "link" attrs _) 
           = case (lookup "rel" attrs, lookup "href" attrs) of
               (Just "import", Just href) -> [href]
               _ -> []
       getLink _
           = []
       links = filter (not . (`elem` prevloads)) $ qquery getLink doc
   loads <- mapM (loadDocument . T.unpack) links
   
   -- we really should be in some error monad here...
   case collect loads of
     Left err -> return $ Left err
     Right docs -> do 
       those <- mapM (lT (links++prevloads)) docs
       case collect those of
         Left err -> return $ Left err
         Right tmpls -> return $ Right $ M.unions $ these:tmpls

collect :: [Either String a] -> Either String [a]
collect es = case partitionEithers es of
               ([], xs) -> Right xs
               (errs, _) -> Left $ unlines errs

runTemplates :: Templates -> Document -> Document
runTemplates ts d = d { docContent = runTemplatesOnNodes ts $ docContent d } where

runTemplatesOnNodes :: Templates -> [Node] -> [Node]
runTemplatesOnNodes ts = traverse where
  traverse :: [Node] -> [Node]
  traverse = map onChildren . subTemplates . filter (not . isLinkOrTemplate) 
  isLinkOrTemplate (Element "template" _ _) = True
  isLinkOrTemplate (Element "link" atts _) = case lookup "rel" atts of
                                               Just "import" -> True
                                               _ -> False
  isLinkOrTemplate _ = False
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
