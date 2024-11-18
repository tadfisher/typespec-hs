{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators #-}

module Language.TypeSpec.Parser.Lexer
  ( Token(..)
  , LocatedToken(..)
  , lex
  , ParseError(..)
  ) where

import Control.Applicative (empty, (<|>))
import Control.Exception.Safe (Exception(..))
import Control.Monad.Combinators (manyTill)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Language.TypeSpec.Parser.Location (Location(..), Offset(..))
import Prelude hiding (lex)
import Text.Megaparsec (ParseErrorBundle(..), try, (<?>), notFollowedBy, MonadParsec, Tokens)

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Language.TypeSpec.Parser.Location as Location
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import Text.Megaparsec.Char (string, char)
import Control.Monad (void)

-- | Short-hand type synonym used by lexing utilities
type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space = Lexer.space
  Megaparsec.Char.space1
  (Lexer.skipLineComment "//")
  (Lexer.skipBlockCommentNested "/*" "*/") -- TODO preserve doc comments

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parseToken :: Parser Token
parseToken =
  Combinators.choice
  [ Combinators.choice
    [ numericLiteral
    , stringLiteral
    ] <?> "literal"

  , Combinators.choice
    [ OpenParen          <$ symbol "("
    , CloseParen         <$ symbol ")"
    , Comma              <$ symbol ","
    , ColonColon         <$ symbol "::"
    , Colon              <$ symbol ":"
    , Semicolon          <$ symbol ";"
    , OpenBracket        <$ symbol "["
    , CloseBracket       <$ symbol "]"
    , OpenBrace          <$ symbol "{"
    , CloseBrace         <$ symbol "}"
    , AtAt               <$ symbol "@@"
    , At                 <$ symbol "@"
    , HashBrace          <$ symbol "${"
    , HashBracket        <$ symbol "$["
    , Hash               <$ symbol "#"
    , Plus               <$ symbol "+"
    , Hyphen             <$ symbol "-"
    , Star               <$ symbol "*"
    , Question           <$ symbol "?"
    , AmpersandAmpersand <$ symbol "&&"
    , Ampersand          <$ symbol "&"
    , Ellipsis           <$ symbol "..."
    , Dot                <$ symbol "."
    , ForwardSlash       <$ symbol "/"
    , LessThanEquals     <$ symbol "<="
    , LessThan           <$ symbol "<"
    , GreaterThanEquals  <$ symbol ">="
    , GreaterThan        <$ symbol ">"
    , EqualsEquals       <$ symbol "=="
    , EqualsGreaterThan  <$ symbol "=>"
    , Equals             <$ symbol "="
    , BarBar             <$ symbol "||"
    , Bar                <$ symbol "|"
    , ExclamationEquals  <$ symbol "!="
    , Exclamation        <$ symbol "!"
    ] <?> "punctuation"

  , Combinators.choice
    [ ImportKeyword    <$ symbol "import"
    , ModelKeyword     <$ symbol "model"
    , ScalarKeyword    <$ symbol "scalar"
    , NamespaceKeyword <$ symbol "namespace"
    , UsingKeyword     <$ symbol "using"
    , OpKeyword        <$ symbol "op"
    , EnumKeyword      <$ symbol "enum"
    , AliasKeyword     <$ symbol "alias"
    , IsKeyword        <$ symbol "is"
    , InterfaceKeyword <$ symbol "interface"
    , UnionKeyword     <$ symbol "union"
    , ElseKeyword      <$ symbol "else"
    , IfKeyword        <$ symbol "if"
    , DecKeyword       <$ symbol "dec"
    , FnKeyword        <$ symbol "fn"
    , ConstKeyword     <$ symbol "const"
    , InitKeyword      <$ symbol "init"
    , ExternKeyword    <$ symbol "extern"
    , ExtendsKeyword   <$ symbol "extends"
    , TrueKeyword      <$ symbol "true"
    , FalseKeyword     <$ symbol "false"
    , ReturnKeyword    <$ symbol "return"
    , VoidKeyword      <$ symbol "void"
    , NeverKeyword     <$ symbol "never"
    , UnknownKeyword   <$ symbol "unknown"
    , ValueOfKeyword   <$ symbol "valueof"
    , TypeOfKeyword    <$ symbol "typeof"
    ] <?> "keyword"

  , identifier
  ]

parseLocatedToken :: Parser LocatedToken
parseLocatedToken = do
    start <- fmap Offset Megaparsec.getOffset
    token <- parseToken
    return LocatedToken{..}

parseLocatedTokens :: Parser [LocatedToken]
parseLocatedTokens = do
    space
    manyTill parseLocatedToken Megaparsec.eof

-- | Lex a complete expression
lex :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError [LocatedToken]
lex name code =
    case Megaparsec.parse parseLocatedTokens name code of
        Left ParseErrorBundle{..} -> do
            let bundleError :| _ = bundleErrors

            let offset = Offset (Error.errorOffset bundleError)

            Left (LexingFailed (Location{..}))
        Right tokens -> do
            return tokens

numericLiteral :: Parser Token
numericLiteral = do
  scientific <- Lexer.signed empty Lexer.scientific
  return (NumericLiteral scientific)

stringLiteral :: Parser Token
stringLiteral =
  try parseMulti <|> parseSingle
  where
    parseMulti = lexeme do
      "\"\"\""
      text <- manyTill Lexer.charLiteral (string "\"\"\"")
      return (StringLiteral (Text.pack text))

    parseSingle = lexeme do
      char '"'
      text <- manyTill Lexer.charLiteral (char '"')
      return (StringLiteral (Text.pack text))

reserved :: HashSet Text
reserved =
  HashSet.fromList
    [ "import"
    , "model"
    , "scalar"
    , "namespace"
    , "using"
    , "op"
    , "enum"
    , "alias"
    , "is"
    , "interface"
    , "union"
    , "else"
    , "if"
    , "dec"
    , "fn"
    , "const"
    , "init"
    , "extern"
    , "extends"
    , "true"
    , "false"
    , "return"
    , "void"
    , "never"
    , "any"
    , "valueof"
    , "typeof"
    ]

identifier :: Parser Token
identifier =
  try parseBackticked <|> parseIdentifier
  where
    parseBackticked = lexeme do
      "`"
      text <- manyTill Lexer.charLiteral (char '`')
      return (Identifier (Text.pack text))

    isIdentifier :: Char -> Bool
    isIdentifier c = Char.isLetter c || Char.isDigit c || c == '$' || c == '_'

    isIdentifier0 :: Char -> Bool
    isIdentifier0 c = isIdentifier c && not (Char.isDigit c)

    parseIdentifier = lexeme do
      c0 <- Megaparsec.satisfy isIdentifier0 <?> "identifier character"
      cs <- Megaparsec.takeWhileP (Just "identifier character") isIdentifier
      let result = Text.cons c0 cs
      Monad.guard (not (HashSet.member result reserved))
      return (Identifier result)

data Token
  = EndOfFile
  | Identifier Text
  | NumericLiteral Scientific
  | StringLiteral Text
  | StringTemplateHead Text
  | StringTemplateMiddle Text
  | StringTemplateTail Text
  -- Trivia
  | SingleLineComment Text
  | MultiLineComment Text
  | NewLine
  | Whitespace
  | ConflictMarker
  -- Doc comment
  | DocText Text
  | DocCodeSpan Text
  | DocCodeFenceDelimiter
  -- Punctuation
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | Dot
  | Ellipsis
  | Semicolon
  | Comma
  | LessThan
  | GreaterThan
  | Equals
  | Ampersand
  | Bar
  | Question
  | Colon
  | ColonColon
  | At
  | AtAt
  | Hash
  | HashBrace
  | HashBracket
  | Star
  | ForwardSlash
  | Plus
  | Hyphen
  | Exclamation
  | LessThanEquals
  | GreaterThanEquals
  | AmpersandAmpersand
  | BarBar
  | EqualsEquals
  | ExclamationEquals
  | EqualsGreaterThan
  -- Keywords
  | ImportKeyword
  | ModelKeyword
  | ScalarKeyword
  | NamespaceKeyword
  | UsingKeyword
  | OpKeyword
  | EnumKeyword
  | AliasKeyword
  | IsKeyword
  | InterfaceKeyword
  | UnionKeyword
  | ProjectionKeyword
  | ElseKeyword
  | IfKeyword
  | DecKeyword
  | FnKeyword
  | ConstKeyword
  | InitKeyword
  | ExternKeyword
  | ExtendsKeyword
  | TrueKeyword
  | FalseKeyword
  | ReturnKeyword
  | VoidKeyword
  | NeverKeyword
  | UnknownKeyword
  | ValueOfKeyword
  | TypeOfKeyword
  deriving stock (Eq, Show)

{-| A token with offset information attached, used for reporting line and
    column numbers in error messages
-}
data LocatedToken = LocatedToken { token :: Token, start :: Offset }
    deriving (Show)

-- | Errors related to lexing and parsing
data ParseError
    = LexingFailed Location
    | ParsingFailed Location
    deriving (Eq, Show)

instance Exception ParseError where
    displayException (LexingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Lexing failed" location)
    displayException (ParsingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Parsing failed" location)
