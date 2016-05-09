module Bustle.Data.User where

import qualified Data.Text as T

-- | Friendly fetchers

-- | User Data Type

data User = User
  { userId     :: PostId
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

-- | User ID Scalar

newtype UserId = UserId Int deriving (Eq, Show, Typeable)

instance GraphQLScalar UserId where
  serialize (UserId x) = serialize x
  deserialize = fmap UserId . deserialize

instance GraphQLValue Haxl UserId
instance GraphQLType SCALAR Haxl UserId where
  def = defineScalar "ID" ""

-- | Haxl implementation

data UserReq a where
  GetUser :: UserId -> UserReq (Maybe User)
  deriving (Typeable)

deriving instance Eq   (UserReq a)
deriving instance Show (UserReq a)

instance Show1 UserReq where show1 = show

instance Hashable (UserReq a) where
  hashWithSalt s (GetUser (UserId i)) = hashWithSalt s (0::Int, i)

-- State store


