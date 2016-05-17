module Bustle.DataSource.Graph where

import           Control.Concurrent.Async (async, wait)
import           Control.Lens
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.AWS
import qualified Data.ByteString     as B
import           Data.Hashable
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (null)
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import           Data.Typeable
import qualified Network.AWS         as AWS
import           Network.AWS.DynamoDB.BatchGetItem
import           Network.AWS.DynamoDB.Types

import qualified Rad.Graph.Structure as Structure

import Bustle.Env

-- | Graph Source implementation

instance Structure.GraphSource Haxl where
  getVertex = dataFetch . GetVertex
  getEdge   = dataFetch . GetEdge

-- | Haxl request handler

data GraphReq a where

  GetVertex    :: Structure.Id
               -> GraphReq Structure.SomeVertex

  GetEdge      :: Structure.Id
               -> GraphReq Structure.SomeEdge

  GetAdjacency :: Structure.Id
               -> Structure.Label
               -> Maybe Structure.Weight
               -> Int
               -> GraphReq [Structure.SomeEdge]

  deriving (Typeable)

deriving instance Eq   (GraphReq a)
deriving instance Show (GraphReq a)

instance Show1 GraphReq where show1 = show

instance Hashable (GraphReq a) where
  hashWithSalt s (GetVertex i) = hashWithSalt s (0::Int, Structure.getId i)
  hashWithSalt s (GetEdge   i) = hashWithSalt s (0::Int, Structure.getId i)

-- State store

instance StateKey GraphReq where
  data State GraphReq = GraphState
    { awsEnv    :: AWS.Env
    , tableName :: T.Text
    }

initState :: AWS.Env -> T.Text -> State GraphReq
initState e n = GraphState
  { awsEnv    = e
  , tableName = n
  }

-- Data source

instance DataSourceName GraphReq where
  dataSourceName _ = "Graph Source (DynamoDB)"

instance DataSource BustleEnv GraphReq where
  fetch st _flags _benv bfs = AsyncFetch $ \inner -> do
      asyncReq <- async $ runResourceT $ AWS.runAWS e $ do
        -- fetch all vertices
        undefined
        -- fetch all edges
        undefined
        -- query all adjacencies
      inner
      wait asyncReq
    where e = awsEnv    st
          t = tableName st

-- | Haxl mutation handler

data GraphMutation a where

  AddVertex :: Structure.Label
            -> Structure.Payload
            -> GraphMutation Structure.Id

  AddEdge   :: Structure.Id
            -> Structure.Label
            -> Structure.Id
            -> Maybe Structure.Weight
            -> Structure.Payload
            -> GraphMutation Structure.Id

  UpdateVertex :: Structure.Id
               -> Structure.Payload
               -> GraphMutation ()

  UpdateEdge   :: Structure.Id
               -> Structure.Payload
               -> GraphMutation ()

  MoveEdge     :: Structure.Id
               -> Structure.Weight
               -> GraphMutation ()

  -- deletions are actually cacheable
  -- but caching a mutation makes me pretty uncomfy
  DeleteVertex :: Structure.Id
               -> GraphMutation ()

  DeleteEdge   :: Structure.Id
               -> GraphMutation ()

  deriving (Typeable)

deriving instance Eq   (GraphMutation a)
deriving instance Show (GraphMutation a)

instance Show1 GraphMutation where show1 = show

instance Hashable (GraphMutation a) where
  hashWithSalt s _ = hashWithSalt s (0::Int)
