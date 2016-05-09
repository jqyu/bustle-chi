module Bustle.Data.User
  -- friendly fetchers
  ( get
  -- types
  , Id(..)
  , User
  , mixin
  -- haxl state initialization
  , initState
  ) where

import           Control.Concurrent.Async (async, wait)
import           Control.Lens
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
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

import qualified Bustle.Data.Post as Post

import Bustle.Env

-- | Friendly fetchers

get :: Id -> Haxl (Maybe User)
get = dataFetch . GetUser

-- | User Data Type

data User = User
  { userId     :: Id
  , name       :: T.Text
  , userRole   :: Int
  , mediaKey   :: Maybe T.Text
  , bio        :: Maybe T.Text
  , twitter    :: Maybe T.Text
  , facebook   :: Maybe T.Text
  , googlePlus :: Maybe T.Text
  , url        :: Maybe T.Text
  } deriving (Eq, Show, Typeable)

instance GraphQLValue Haxl User
instance GraphQLType OBJECT Haxl User where

  def = defineObject "User" $ do

    describe "A user, stored in dynamodb"

    field "id"         $ resolve $~> userId
    field "name"       $ resolve $~> name
    field "role"       $ resolve $~> userRole
    field "mediaKey"   $ resolve $~> mediaKey
    field "bio"        $ resolve $~> bio
    field "twitter"    $ resolve $~> twitter
    field "facebook"   $ resolve $~> facebook
    field "googlePlus" $ resolve $~> googlePlus
    field "url"        $ resolve $~> url

    field "posts" $ do
      describe "all posts published by this author"
      limit  <- arg "limit"  |= 30
      offset <- arg "offset" |= 0
      resolve $->> \args v ->
        let uid = fromIntegral . unwrapId . userId $ v :: Double
            lim = limit  args
            ofs = offset args
         in Post.range Post.AUTHOR_ID uid uid lim ofs
            >>= traverse Post.get

-- | Root query mixin

mixin :: ObjectFragment Haxl a
mixin = do

  describe "Implements the User mixin"

  field "user" $ do
    describe "Retrieves a single user by id"
    userId <- arg "id"
    resolve *->> get . userId

-- | User ID Scalar

newtype Id = Id { unwrapId :: Int } deriving (Eq, Show, Typeable)

instance GraphQLScalar Id where
  serialize (Id x) = serialize x
  deserialize = fmap Id . deserialize

instance GraphQLValue Haxl Id
instance GraphQLType SCALAR Haxl Id where
  def = defineScalar "ID" ""

-- | Haxl implementation

data UserReq a where
  GetUser :: Id -> UserReq (Maybe User)
  deriving (Typeable)

deriving instance Eq   (UserReq a)
deriving instance Show (UserReq a)

instance Show1 UserReq where show1 = show

instance Hashable (UserReq a) where
  hashWithSalt s (GetUser (Id i)) = hashWithSalt s (0::Int, i)

-- State store
instance StateKey UserReq where
  data State UserReq = UserState
    { awsEnv    :: AWS.Env
    , tableName :: T.Text
    }

initState :: AWS.Env -> T.Text -> State UserReq
initState e n = UserState
  { awsEnv    = e
  , tableName = n
  }

-- Data source

instance DataSourceName UserReq where
  dataSourceName _ = "User Source (DynamoDB)"

instance DataSource BustleEnv UserReq where
  fetch st _flags _benv bfs = AsyncFetch $ \inner -> do
      asyncReq <- async $ runResourceT $ AWS.runAWS e $ do
        let userReqs = map getUserReqs bfs
        unless (null userReqs) $ do
          let kaa = keysAndAttributes
                  $ NonEmpty.fromList
                  $ map toDynamoKey userReqs
              bgi = batchGetItem
                  & bgiRequestItems
                 .~ HashMap.singleton t kaa
          res <- AWS.send bgi
          mapM_ (resolveUserReq $ HashMap.fromList userReqs)
              $ (res^.bgirsResponses) ! t
      inner
      wait asyncReq
    where e = awsEnv    st
          t = tableName st

-- auto-gen bullshit handling

getUserReqs :: BlockedFetch UserReq -> (Int, ResultVar (Maybe User))
getUserReqs (BlockedFetch (GetUser (Id i)) rvar) = (i, rvar)

toDynamoKey :: (Int, a) -> HashMap T.Text AttributeValue
toDynamoKey (i, _) = HashMap.singleton "id"
                   $ attributeValue & avN .~ (Just $ T.pack $ show i)

resolveUserReq :: (MonadIO m)
               => HashMap Int (ResultVar (Maybe User))
               -> HashMap T.Text AttributeValue
               -> m ()
resolveUserReq rvars attrs
    | Just i <- attr "id" avN >>= parseInt =
        liftIO $ putSuccess (rvars ! i)
               $ makeU i <$> (attr "name" avS)
                         <*> (attr "role" avN >>= parseInt)
    | otherwise = return ()
  where attr k l = HashMap.lookup k attrs >>= (^. l)
        makeU i n r = User
          { userId     = Id i
          , name       = n
          , userRole   = r
          , mediaKey   = attr "media_key"  avS
          , bio        = attr "bio"        avS
          , twitter    = attr "twitter"    avS
          , facebook   = attr "facebook"   avS
          , googlePlus = attr "googlePlus" avS
          , url        = attr "url"        avS
          }

parseInt :: T.Text -> Maybe Int
parseInt t = case TR.decimal t of
                  Right (i, _) -> Just i
                  _            -> Nothing
