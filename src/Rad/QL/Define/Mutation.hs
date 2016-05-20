module Rad.QL.Define.Mutation where

import Rad.QL.Internal.Builders
import Rad.QL.Internal.Types

import Rad.QL.Define.Field
import Rad.QL.Define.Util

import Rad.QL.AST
import Rad.QL.Query
import Rad.QL.Types

data Foo = Foo

data GraphQLMutationDef m a = GraphQLMutationDef
  { payloadDef :: Foo
  , inputDef   :: Foo
  , mutation   :: Foo -> m a
  }

defineMutation :: (Monad m) => Name -> MutationDefM m a b -> GraphQLMutationDef m a
defineMutation n def = undefined

data MutationDefM m a b = MutationDefM
  { mdDesc :: Description
  , unwrap :: b
  }

instance Functor (MutationDefM m a) where
  fmap f x = pure f <*> x

instance Applicative (MutationDefM m a) where
  pure x = MutationDefM
    { mdDesc = ""
    , unwrap = x
    }
  f <*> x = ObjectDefM
    { mdDesc = mdDesc f <> mdDesc x
    , unwrap = unwrap f $  unwrap x
    }

instance Monad (MutationDefM m a) where
  m >>= k = m >> k (unwrap m)
  m >>  k = m { unwrap = id } <*> k

-- Woop woop

data TextCard = TextCard
  { text :: Int
  }

data CreatePayload = CreatePayload
  { node :: Vertex TextCard
  }

createMutation :: GraphQLMutationDef Haxl CreatePayload
createMutation = defineMutation "createTextCard" $ do
  -- gather arguments
  p  <- arg "text" |= ""
  me <- session
  -- create payload
  let attrs = TextCard { text = p }
  -- push vertex to store
  v  <- G.addV attrs
  -- return payload
  return $ CreatePayload { node = v }

-- example query
-- mutation {
--   createTextCard
--   ( clientMutationId: $someId
--   , input: { text: "my text" }
--   ) {
--     clientMutationId
--     node {
--       id
--       text
--     }
--   }
-- }
