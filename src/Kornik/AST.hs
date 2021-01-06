module Kornik.AST where

import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Data.Aeson
  (FromJSON, parseJSON, withObject, (.:), (.:?), (.!=), eitherDecodeFileStrict)
import Language.Haskell.TH
  ( Dec(..)
  , Clause(..)
  , Body(..)
  , Exp(..)
  , Lit(..)
  , Pat(..)
  , Name
  , mkName
  , Con(..)
  , DerivClause(..)
  , Type(..)
  , Bang(..)
  , SourceUnpackedness(..)
  , SourceStrictness(..)
  , Q
  , runIO
  )
import Text.Casing (fromAny, toCamel)
import Kornik.Proxy (ProxyNode(..))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)



data TreeSitterNodeType = TreeSitterNodeType
  { _type     :: String
  , _named    :: Bool
  , _subtypes :: [TreeSitterNodeRef]
  , _children :: Maybe TreeSitterNodeField
  , _fields   :: HashMap String TreeSitterNodeField
  }
  deriving (Generic, Show)

data TreeSitterNodeRef = TreeSitterNodeRef
  { _type  :: String
  , _named :: Bool
  }
  deriving (Generic, Show)

data TreeSitterNodeField = TreeSitterNodeField
  { _multiple :: Bool
  , _required :: Bool
  , _types    :: [TreeSitterNodeRef]
  }
  deriving (Generic, Show)

instance FromJSON TreeSitterNodeType where
  parseJSON = withObject "NodeType" $ \o -> do
    _type     <- o .: "type"
    _named    <- o .: "named"
    _subtypes <- o .:? "subtypes" .!= []
    _children <- o .:? "children"
    _fields   <- o .:? "fields" .!= mempty
    return TreeSitterNodeType { .. }

instance FromJSON TreeSitterNodeRef where
  parseJSON = withObject "NodeRef" $ \o -> do
    _type  <- o .: "type"
    _named <- o .: "named"
    return TreeSitterNodeRef { .. }

instance FromJSON TreeSitterNodeField where
  parseJSON = withObject "NodeField" $ \o -> do
    _multiple <- o .: "multiple"
    _required <- o .: "required"
    _types    <- o .:? "types" .!= []
    return TreeSitterNodeField { .. }


loadTreeSitterNodeTypes :: FilePath -> IO [TreeSitterNodeType]
loadTreeSitterNodeTypes path =
  either fail return =<< eitherDecodeFileStrict path


filterFinalTreeSitterNodeTypes :: [TreeSitterNodeType] -> [TreeSitterNodeType]
filterFinalTreeSitterNodeTypes = filter isFinal


isFinal :: TreeSitterNodeType -> Bool
isFinal TreeSitterNodeType {..} = _named && null _subtypes

isLeaf :: TreeSitterNodeType -> Bool
isLeaf TreeSitterNodeType {..} =
  _named && null _subtypes && null _children && null _fields

sanitizedName :: String -> Name
sanitizedName name = mkName $ "AST_" ++ toCamel (fromAny name)

mkAstConE :: TreeSitterNodeType -> Exp
mkAstConE nodeType@TreeSitterNodeType { _type }
  | isLeaf nodeType = AppE
    (ConE $ sanitizedName _type)
    (AppE (VarE 'decodeUtf8) (VarE $ mkName "p"))
  | otherwise = AppE
    (ConE $ sanitizedName _type)
    (AppE (AppE (VarE 'map) (VarE $ mkName "buildAst")) (VarE $ mkName "c"))

mkAstOtherConE :: Exp
mkAstOtherConE = AppE
  (AppE (ConE $ mkName "AST__OTHER") (AppE (VarE 'pack) (VarE $ mkName "t")))
  (AppE (VarE 'decodeUtf8) (VarE $ mkName "p"))

mkAstCon :: TreeSitterNodeType -> Con
mkAstCon nodeType@TreeSitterNodeType { _type }
  | isLeaf nodeType = NormalC
    (sanitizedName _type)
    [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)]
  | otherwise = NormalC
    (sanitizedName _type)
    [ ( Bang NoSourceUnpackedness NoSourceStrictness
      , AppT ListT (ConT (mkName "AST"))
      )
    ]

mkAstOtherCon :: Con
mkAstOtherCon = NormalC
  (mkName "AST__OTHER")
  [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
  , (Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
  ]

mkAstDataD :: [TreeSitterNodeType] -> Dec
mkAstDataD nodeTypes = DataD
  []
  (mkName "AST")
  []
  Nothing
  (map mkAstCon nodeTypes <> [mkAstOtherCon])
  [DerivClause Nothing [ConT ''Show]]


mkMapperClause :: TreeSitterNodeType -> Clause
mkMapperClause nodeType@TreeSitterNodeType { _type } = Clause
  [ ConP
      'ProxyNode
      [TupP [LitP (StringL _type), VarP (mkName "p"), VarP (mkName "c")]]
  ]
  (NormalB $ mkAstConE nodeType)
  []

mkMapperOtherClause :: Clause
mkMapperOtherClause = Clause
  [ ConP
      'ProxyNode
      [TupP [VarP (mkName "t"), VarP (mkName "p"), VarP (mkName "c")]]
  ]
  (NormalB mkAstOtherConE)
  []

mkMapperFunD :: [TreeSitterNodeType] -> Dec
mkMapperFunD nodeTypes = FunD
  (mkName "buildAst")
  (map mkMapperClause nodeTypes <> [mkMapperOtherClause])

mkMapperSigD :: Dec
mkMapperSigD = SigD
  (mkName "buildAst")
  (AppT (AppT ArrowT (ConT ''ProxyNode)) (ConT $ mkName "AST"))

mkAstDef :: FilePath -> Q [Dec]
mkAstDef path = do
  nodeTypes <-
    runIO $ filterFinalTreeSitterNodeTypes <$> loadTreeSitterNodeTypes path
  return [mkMapperSigD, mkAstDataD nodeTypes, mkMapperFunD nodeTypes]
