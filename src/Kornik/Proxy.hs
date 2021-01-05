module Kornik.Proxy
  ( ProxyNode(..)
  , parse
  , parseFile
  )
where

import Prelude
  ( String
  , Show
  , IO
  , Bool(..)
  , (>>=)
  , return
  , fail
  , ($)
  , fromIntegral
  , (-)
  , (<>)
  , FilePath
  )
import TreeSitter.Language (Language)
import Foreign.Ptr (Ptr)
import TreeSitter.Parser (withParser, withParseTree)
import TreeSitter.Tree (withRootNode)
import TreeSitter.Cursor
  ( withCursor
  , ts_tree_cursor_current_node_p
  , ts_tree_cursor_goto_first_child
  , ts_tree_cursor_goto_parent
  , ts_tree_cursor_goto_next_sibling
  )
import Foreign (castPtr, peek)
import Foreign.C (peekCString)
import TreeSitter.Node (nodeType, nodeStartByte, nodeEndByte)
import Data.ByteString (drop, take, ByteString, readFile)

-- |Untyped intermediate syntax tree.
newtype ProxyNode
  = ProxyNode (String, ByteString, [ProxyNode])
  deriving (Show)

-- |Parses given `content` according to `language` into untyped intermediate syntax tree. 
parse :: Ptr Language -> ByteString -> IO [ProxyNode]
parse language content = withParser language $ \parser ->
  withParseTree parser content $ \tree -> withRootNode tree $ \node ->
    withCursor (castPtr node) $ \cursor ->
      let
        walk :: ([ProxyNode] -> IO [ProxyNode]) =
          (\acc -> do
            _         <- ts_tree_cursor_current_node_p cursor node
            node'     <- peek node

            children' <- ts_tree_cursor_goto_first_child cursor >>= \case
              True -> do
                children <- walk []
                ts_tree_cursor_goto_parent cursor >>= \case
                  True  -> return children
                  False -> fail "Can't reach parent"
              False -> return []

            proxyNode <- do
              nodeType' <- peekCString $ nodeType node'
              let nodeStartByte' = fromIntegral $ nodeStartByte node'
              let nodeEndByte'   = fromIntegral $ nodeEndByte node'
              let
                nodeContent' = take
                  (nodeEndByte' - nodeStartByte')
                  (drop nodeStartByte' content)
              return $ ProxyNode (nodeType', nodeContent', children')

            ts_tree_cursor_goto_next_sibling cursor >>= \case
              True  -> walk $ acc <> [proxyNode]
              False -> return $ acc <> [proxyNode]
          )
      in walk []

-- |Parses file on given `patch` according to `language` into untyped intermediate syntax tree.  
parseFile :: Ptr Language -> FilePath -> IO [ProxyNode]
parseFile language patch = readFile patch >>= parse language
