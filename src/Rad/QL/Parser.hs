-- TODO: Parsing validation
-- thread a type dictionary through the parser
-- to determine the AST only produces valid queries

module Rad.QL.Parser
  ( parseDocument
  ) where

import Prelude hiding (takeWhile)

import           Control.Applicative (Alternative((<|>), empty))
import           Control.Monad (when)
import qualified Data.Aeson.Parser as JSON
import Data.Attoparsec.ByteString
  ( Parser
  , IResult(..)
  , parse
  , (<?>)
  , word8
  , satisfy
  , anyWord8
  , peekWord8
  , string
  , takeTill
  , takeWhile
  , many'
  , many1
  , option
  )
import Data.Attoparsec.ByteString.Char8
  ( signed
  , double
  )

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Foldable (traverse_)
import qualified Data.Text.Encoding as TE
import           Data.Word8
  ( Word8
  , isSpace
  , isAlpha
  , isDigit
  , _at
  , _colon
  , _comma
  , _dollar
  , _exclam
  , _equal
  , _numbersign
  , _underscore
  , _parenleft  , _parenright
  , _bracketleft, _bracketright
  , _braceleft  , _braceright
  )

import Rad.QL.AST

parseDocument :: ByteString -> Either String Document
parseDocument q = 
  case parse document q of
       Done _ ast -> Right ast
       Fail _ _ e -> Left e
       Partial k  ->
         case k "" of
              Done _ ast -> Right ast
              Fail _ _ e -> Left e
              _          -> Left "something really fucked up with the parser"

-- parse a name

isaz_ :: Word8 -> Bool
isaz_ = (||) <$> isAlpha <*> (== _underscore)

name :: Parser Name
name = tok $ B.cons
         <$> satisfy isaz_
         <*> takeWhile ((||) <$> isDigit <*> isaz_)

-- parse a document

document :: Parser Document
document = whiteSpace
        *> (Document <$> many1 definition)
       <|> (Document . pure
                     . DefOperation
                     . Query
                     . Node mempty empty empty
                   <$> selectionSet
           )
       <?> "document error!"

definition :: Parser Definition
definition = DefOperation <$> operationDef
         <|> DefFragment  <$> fragmentDef
         <?> "definition error!"

operationDef :: Parser OperationDef
operationDef = Query    <$ sym "query"    <*> node
           <|> Mutation <$ sym "mutation" <*> node
           <?> "operationDef error!"

node :: Parser Node
node = Node <$> name
            <*> optempty variableDefs
            <*> optempty directives
            <*> selectionSet

variableDefs :: Parser [VariableDef]
variableDefs = parens $ many1 variableDef

variableDef :: Parser VariableDef
variableDef = VariableDef <$> variable
                          <*  wrd _colon
                          <*> type_
                          <*> optional defaultValue

defaultValue :: Parser DefaultValue
defaultValue = wrd _equal *> value

variable :: Parser Variable
variable = Variable <$  wrd _dollar
                    <*> name

selectionSet :: Parser SelectionSet
selectionSet = braces $ many1 selection

selection :: Parser Selection
selection = SelectionField          <$> field
        <|> SelectionInlineFragment <$> inlineFragment
        <|> SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

field :: Parser Field
field = Field <$> optempty alias
              <*> name
              <*> optempty arguments
              <*> optempty directives
              <*> optempty selectionSet

alias :: Parser Alias
alias = name <* wrd _colon

arguments :: Parser [Argument]
arguments = parens $ many1 argument

argument :: Parser Argument
argument = Argument <$> name
                    <*  wrd _colon
                    <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread <$  sym "..."
                                <*> name
                                <*> optempty directives

inlineFragment :: Parser InlineFragment
inlineFragment = InlineFragment <$  sym "..."
                                <*  sym "on"
                                <*> typeCondition -- TODO: implement monoid, this can be empty
                                <*> optempty directives
                                <*> selectionSet

fragmentDef :: Parser FragmentDef
fragmentDef = FragmentDef <$  sym "fragment"
                          <*> name
                          <*  sym "on"
                          <*> typeCondition
                          <*> optempty directives
                          <*> selectionSet

typeCondition :: Parser TypeCondition
typeCondition = namedType

-- * Values

value :: Parser Value
value = ValueVariable <$> variable
    <|> floatOrInt    <$> tok (signed double) -- HACKY, FIX
    <|> ValueBoolean  <$> booleanValue
    <|> ValueString . TE.encodeUtf8 <$> tok JSON.jstring -- TODO: revisit this so there's less back-and-forth casting
    <|> ValueEnum     <$> name
    <|> ValueList     <$> listValue
    <|> ValueObject   <$> objectValue
    <?> "value error!"

-- THIS IS HACKY, FIX IT
floatOrInt :: Double -> Value
floatOrInt x | flr == ceil = ValueInt flr
             | otherwise   = ValueFloat x
  where flr = floor x :: Int
        ceil  = ceiling x :: Int

booleanValue :: Parser Bool
booleanValue = True  <$ sym "true"
           <|> False <$ sym "false"

listValue :: Parser ListValue
listValue = ListValue <$> brackets (many' value)

objectValue :: Parser ObjectValue
objectValue = ObjectValue <$> braces (many' objectField)

objectField :: Parser ObjectField
objectField = ObjectField <$> name
                          <*  wrd _colon
                          <*> value

-- * Directives

-- why is this a many1 when directives are always optional?
directives :: Parser [Directive]
directives = many1 directive

directive :: Parser Directive
directive = Directive <$  wrd _at
                      <*> name
                      <*> optempty arguments

-- * Type References

type_ :: Parser Type
type_ = TypeList    <$> listType
    <|> TypeNonNull <$> nonNullType
    <|> TypeNamed   <$> namedType
    <?> "type error!"

namedType :: Parser NamedType
namedType = NamedType <$> name

listType :: Parser ListType
listType = ListType <$> brackets type_

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> namedType <* wrd _exclam
          <|> NonNullTypeList  <$> listType  <* wrd _exclam
          <?> "non null type error"

-- * Tokenizer

tok :: Parser a -> Parser a
tok p = p <* whiteSpace

sym :: ByteString -> Parser ByteString
sym = tok . string

wrd :: Word8 -> Parser Word8
wrd = tok . word8

parens   :: Parser a -> Parser a
parens   = between   _parenleft   _parenright

brackets :: Parser a -> Parser a
brackets = between _bracketleft _bracketright

braces   :: Parser a -> Parser a
braces   = between   _braceleft   _braceright

between :: Word8 -> Word8 -> Parser a -> Parser a
between open close p = wrd open *> p <* wrd close

optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty

optional :: Parser a -> Parser (Maybe a)
optional = option Nothing . (Just <$>)

whiteSpace :: Parser ()
whiteSpace = peekWord8 >>= traverse_ (\c ->
    if insignificant c
       then anyWord8 *> whiteSpace
       else when (c == _numbersign) $ takeTill endOfLine *> whiteSpace
  )

insignificant :: Word8 -> Bool
insignificant c = isSpace c || c == _comma

endOfLine :: Word8 -> Bool
endOfLine w = w == 13 || w == 10
-- faster than endOfLine = inClass "\r\n"
