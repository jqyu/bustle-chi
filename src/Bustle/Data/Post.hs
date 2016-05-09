module Bustle.Data.Post where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson            as JSON
import qualified Data.ByteString       as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC8
import           Data.Foldable         (traverse_)
import           Data.Hashable
import           Data.Monoid           ((<>))
import qualified Data.Text.Encoding    as TE
import           Data.Typeable
import           Database.Redis        hiding (get)

-- source pragma required to break cycles
import {-# SOURCE #-} qualified Bustle.Data.User as User

import Bustle.Env

-- | Friendly fetchers

get :: Id -> Haxl (Maybe Post)
get = dataFetch . GetPost

all :: Int -> Int -> Haxl [Id]
all x = dataFetch . GetIndex ID x

index :: PostIndex -> Int -> Int -> Haxl [Id]
index x y = dataFetch . GetIndex x y

range :: PostIndex -> Double -> Double -> Int -> Int -> Haxl [Id]
range x y z w = dataFetch . GetRange x y z w

-- | Post Data Type

data Post = Post
  { postId       :: Id
  , title        :: B.ByteString
  , slug         :: B.ByteString
  , bodies       :: RawJSON
  , primaryMedia :: RawJSON
  , postType     :: Int
  , state        :: Maybe Int
  , rating       :: Maybe Int
  , authorId     :: Int
  , publishedAt  :: Maybe Int
  , publication  :: Int
  } deriving (Eq, Show, Typeable)

instance GraphQLValue Haxl Post
instance GraphQLType OBJECT Haxl Post where

  def = defineObject "Post" $ do

    describe "A typeset post"

    field "id"          $ resolve $~> postId
    field "title"       $ resolve $~> title
    field "slug"        $ resolve $~> slug
    field "type"        $ resolve $~> postType
    field "state"       $ resolve $~> state
    field "rating"      $ resolve $~> rating
    field "publishedAt" $ resolve $~> publishedAt
    field "publication" $ resolve $~> publication

    field "bodies" $ do
      describe "An list of mobiledoc payloads"
      resolve $~> bodies

    field "primaryMedia" $ do
      describe "A direct object dump of the primary media"
      resolve $~> primaryMedia

    field "author" $ do
      describe "The user who made the post"
      resolve $~>> User.get . User.Id . authorId

-- | Root query mixin

mixin :: ObjectFragment Haxl a
mixin = do

  describe "Implements the Post mixin"

  field "post" $ do
    describe "Retrieves a single post by id"
    postId <- arg "id"
    resolve *->> get . postId

  field "posts" $ do
    describe "Retrieves the latest posts"
    idx    <- arg "index"  |= PUBLISHED_AT @>
      describe
        $.. "determines which index to query, defaults to PUBLISHED_AT"
        |.. "TODO: restrict other indices for authenticated users only"
    limit  <- arg "limit"  |= 30
    offset <- arg "offset" |= 0
    resolve *->> \args -> do
      ids <- index (idx args) (limit args) (offset args)
      traverse get ids

  field "postsBy" $ do
    describe "Retrieves posts by a given author"
    authorId <- arg "authorId"
    limit    <- arg "limit"  |= 30
    offset   <- arg "offset" |= 0
    resolve *->> \args -> do
      let uid = fromIntegral (authorId args :: Int) :: Double
      ids <- range AUTHOR_ID uid uid (limit args) (offset args)
      traverse get ids

-- | Utility Scalars

newtype Id = Id { unwrapId :: B.ByteString } deriving (Eq, Show, Typeable)

instance GraphQLScalar Id where
  serialize (Id x) = serialize x
  deserialize = fmap Id . deserialize

instance GraphQLValue Haxl Id
instance GraphQLType SCALAR Haxl Id where
  def = defineScalar "ID" ""

-- Raw JSON Scalar (because we don't need to parse the JSON)
newtype RawJSON = RawJSON B.ByteString deriving (Eq, Show, Typeable)

instance GraphQLScalar RawJSON where
  serialize (RawJSON "") = byteString "null"
  serialize (RawJSON  x) = byteString x
  deserialize _ = Nothing

instance GraphQLValue Haxl RawJSON
instance GraphQLType SCALAR Haxl RawJSON where
  def = defineScalar "RawJSON" "raw JSON payload, unsafe but efficient"

-- | Haxl implementation

-- Index ADT
data PostIndex = ID
               | CREATED_AT
               | UPDATED_AT
               | PUBLISHED_AT
               | AUTHOR_ID
               deriving (Eq, Show, Typeable, Generic)

instance GraphQLScalar PostIndex
instance GraphQLValue Haxl PostIndex
instance GraphQLType ENUM Haxl PostIndex where
  def = defineEnum "PostIndex" $
    describe "Enum of possible indices to query posts from"

instance Hashable PostIndex where
  hashWithSalt s ID           = hashWithSalt s (0 :: Int)
  hashWithSalt s CREATED_AT   = hashWithSalt s (1 :: Int)
  hashWithSalt s UPDATED_AT   = hashWithSalt s (2 :: Int)
  hashWithSalt s PUBLISHED_AT = hashWithSalt s (3 :: Int)
  hashWithSalt s AUTHOR_ID    = hashWithSalt s (4 :: Int)

indexKey :: PostIndex -> B.ByteString
indexKey ID           = "id"
indexKey CREATED_AT   = "created_at"
indexKey UPDATED_AT   = "updated_at"
indexKey PUBLISHED_AT = "published_at"
indexKey AUTHOR_ID    = "author_id"

-- Request GADT

data PostReq a where
  GetPost  :: Id                                          -> PostReq (Maybe Post)
  GetIndex :: PostIndex                     -> Int -> Int -> PostReq [Id]
  GetRange :: PostIndex -> Double -> Double -> Int -> Int -> PostReq [Id]
  deriving (Typeable)

deriving instance Eq   (PostReq a)
deriving instance Show (PostReq a)

instance Show1 PostReq where show1 = show

instance Hashable (PostReq a) where
  hashWithSalt s (GetPost  (Id i)       ) = hashWithSalt s (0::Int, i)
  hashWithSalt s (GetIndex idx       l o) = hashWithSalt s (1::Int, idx,         l, o)
  hashWithSalt s (GetRange idx mi ma l o) = hashWithSalt s (2::Int, idx, mi, ma, l, o)

-- State store

instance StateKey PostReq where
  data State PostReq = PostState
    { conn      :: Connection
    , keyPrefix :: B.ByteString
    }

initState :: Connection -> B.ByteString -> State PostReq
initState c kp = PostState { conn = c, keyPrefix = kp }

-- Data source

instance DataSourceName PostReq where
  dataSourceName _ = "Post Source (Radredis)"

instance DataSource BustleEnv PostReq where
  fetch st _flags _benf bfs = AsyncFetch $ \inner -> do
      asyncReq <- async $ runRedis c 
                        $ traverse_ (fetchReq kp) bfs
      inner
      wait asyncReq
    where c  = conn st
          kp = keyPrefix st

fetchReq :: B.ByteString -> BlockedFetch PostReq -> Redis ()
fetchReq kp (BlockedFetch (GetPost (Id i)) rvar) = do
    res <- hmget (kp <> i <> ":attributes")
                 [ "id"           , "title"         , "slug"
                 , "bodies"       , "primary_media" , "type"
                 , "state"        , "rating"        , "author_id"
                 , "published_at" , "publication"
                 ]
    putRes rvar $ parsePost <$> res
  -- do they give awards for obfuscated haskell?
  where parsePost [ postId       , title          , slug
                  , bodies       , primaryMedia   , postType
                  , state        , rating         , authorId
                  , publishedAt  , publication
                  ]
          = Post <$> (Id            <$> postId)
                 <*> title
                 <*> slug
                 <*> (RawJSON       <$> bodies)
                 <*> (RawJSON       <$> primaryMedia)
                 <*> (postType      >>= toInt)
                 <*> (toInt         <$> state)
                 <*> (toInt         <$> rating)
                 <*> (authorId      >>= toInt)
                 <*> (toInt         <$> publishedAt)
                 <*> (publication   >>= toInt)
        parsePost _ = Nothing

fetchReq kp (BlockedFetch (GetIndex idx limit offset) rvar) = do
    res <- toIds <$> zrevrange (kp <> "indexes:" <> indexKey idx) start stop
    putRes rvar res
  where start = toInteger offset
        stop  = toInteger (offset + limit - 1)
        toIds = fmap $ map Id
fetchReq kp (BlockedFetch (GetRange idx zmin zmax limit offset) rvar) = do
    res <- toIds <$> zrevrangebyscoreLimit
              (kp <> "indexes:" <> indexKey idx)
              zmax
              zmin
              offset'
              limit'
    putRes rvar res
  where offset' = toInteger offset
        limit'  = toInteger limit
        toIds   = fmap $ map Id

-- utility functions

putRes :: ResultVar a -> Either Reply a -> Redis ()
putRes rvar (Left ex) = liftIO $ putFailure rvar (replyToException ex)
putRes rvar (Right a) = liftIO $ putSuccess rvar a

replyToException :: Reply -> SomeException
replyToException (Error e) = SomeException $ FetchError $ TE.decodeUtf8 e
replyToException _         = SomeException $ FetchError "what the fuck"

toInt :: B.ByteString -> Maybe Int
toInt n = case BC8.readInt n of
               Just (n', _) -> Just n'
               Nothing      -> Nothing
