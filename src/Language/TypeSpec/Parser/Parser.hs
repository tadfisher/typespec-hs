{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module Language.TypeSpec.Parser.Parser
  ( parse
  , ParseError(..)
  ) where

import Control.Applicative (many, optional, (<|>))
import Control.Applicative.Combinators (sepBy, between)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.Functor (void, ($>))
import Data.List.NonEmpty (head, toList)
import Data.Scientific (Scientific)
import Data.String.Interpolate ( i )
import Data.Text (Text)
import Language.TypeSpec.Parser.Lexer (LocatedToken(LocatedToken), ParseError(..), Token)
import Language.TypeSpec.Parser.Location (Location(..), Offset(..))
import Language.TypeSpec.Syntax hiding (head, optional)
import Text.Earley (Grammar, Prod, Report(..), rule)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Language.TypeSpec.Parser.Lexer as Lexer
import qualified Text.Earley as Earley
import Prelude hiding (head)
import Control.Monad.Combinators (option)
import Data.Maybe (isJust)

data Ann p = Ann
  { docs :: [DocNode p]
  , directives :: [DirectiveExpressionNode p]
  , decorators :: [DecoratorExpressionNode p]
  }
  deriving (Eq, Show)

ann :: Ann Offset
ann = Ann [] [] []

data Meta = Meta
  { location :: Offset
  , annotations :: Ann Offset
  , error :: Maybe String
  }
  deriving (Eq, Show)

meta :: Meta
meta = Meta
  { location = Offset 0
  , annotations = ann
  , error = Nothing
  }

type Parser r = Prod r Text LocatedToken

matchBool :: Token -> Maybe Bool
matchBool Lexer.TrueKeyword  = Just True
matchBool Lexer.FalseKeyword = Just False
matchBool _                  = Nothing

matchIdent :: Token -> Maybe Text
matchIdent (Lexer.Identifier t) = Just t
matchIdent _                    = Nothing

matchNumber :: Token -> Maybe Scientific
matchNumber (Lexer.NumericLiteral s) = Just s
matchNumber _                        = Nothing

matchString :: Token -> Maybe Text
matchString (Lexer.StringLiteral t) = Just t
matchString _                       = Nothing

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken_ = match (Lexer.token locatedToken_)

bool :: Parser r Bool
bool = terminal matchBool

ident :: Parser r Text
ident = terminal matchIdent

number :: Parser r Scientific
number = terminal matchNumber

string :: Parser r Text
string = terminal matchString

token :: Token -> Parser r ()
token t = void (Earley.satisfy predicate)
  where
    predicate locatedToken_ = Lexer.token locatedToken_ == t

locatedBool :: Parser r (Offset, Bool)
locatedBool = locatedTerminal matchBool

locatedIdent :: Parser r (Offset, Text)
locatedIdent = locatedTerminal matchIdent

locatedNumber :: Parser r (Offset, Scientific)
locatedNumber = locatedTerminal matchNumber

locatedString :: Parser r (Offset, Text)
locatedString = locatedTerminal matchString

locatedTerminal :: (Token -> Maybe a) -> Parser r (Offset, a)
locatedTerminal match = Earley.terminal match'
  where
    match' locatedToken_@LocatedToken{ start }  = do
      a <- match (Lexer.token locatedToken_)
      return (start, a)

locatedToken :: Token -> Parser r Offset
locatedToken expectedToken =
    Earley.terminal capture
  where
    capture LocatedToken{ Lexer.token = actualToken, .. }
        | expectedToken == actualToken = Just start
        | otherwise                    = Nothing

bareIdentifier :: Expression p -> Maybe (IdentifierNode p)
bareIdentifier (TypeReferenceE (TypeReferenceNode{..})) =
  if null arguments
    then case target of
      MemberME t -> if length t.ids == 1
                    then Just (head t.ids)
                    else Nothing
      _          -> Nothing
    else Nothing
bareIdentifier _ = Nothing

grammar :: Grammar r (Parser r (Node Meta))
grammar = mdo
  node <- rule $
        (StatementN <$> statement) <|> (ExpressionN <$> expression)

  memberExpression <- rule do
    ids <- sepBy1 identifier (token Lexer.Dot)
    let IdentifierNode{ext=Meta{location}} = head ids
    let base = MemberNode meta{location} ids
    option (MemberME base) do
      token Lexer.ColonColon
      MetaMemberME . MetaMemberNode meta{location} base <$> identifier

  typeOfExpression <- rule do
    location <- locatedToken Lexer.TypeOfKeyword
    TypeOfExpressionNode meta{location} <$> typeOfTargetExpression

  typeOfTargetExpression <- rule
    (   TypeOfE <$> typeOfExpression
    <|> CallE <$> callExpression
    <|> TypeReferenceE <$> typeReference
    <|> StringE <$> stringLiteral
    -- TODO stringTemplateExpression
    <|> BooleanE <$> booleanLiteral
    <|> NumericE <$> numericLiteral
    <|> between (token Lexer.OpenParen) (token Lexer.CloseParen) typeOfTargetExpression
    )

  callExpression <- rule do
    target <- memberExpression
    CallExpressionNode target.ext target <$> functionArgumentList

  typeReference <- rule do
    target <- memberExpression
    TypeReferenceNode target.ext target <$> option [] templateArgumentList

  functionArgumentList <- rule do
    between (token Lexer.OpenParen) (token Lexer.CloseParen) do
      exprs <- expression `sepBy` token Lexer.Comma
      optional (token Lexer.Comma)
      return exprs

  functionParameter <- rule do
    rest <- isJust <$> optional (token Lexer.Ellipsis)
    id_ <- identifier
    optional_ <- isJust <$> optional (token Lexer.Question)
    type_ <- optional do
      token Lexer.Colon
      return mixedParameterConstraint
    return $ FunctionParameterNode id_.ext id_ optional_ rest type_

  functionParameterList <- rule $
    between (token Lexer.OpenParen) (token Lexer.CloseParen) $
      functionParameter `sepBy` token Lexer.Comma

  templateArgument <- rule do
     expr <- expression
     option (TemplateArgumentNode expr.ext Nothing expr) do
       token Lexer.Equals
       let bareId = bareIdentifier expr
       case bareId of
         Just name -> TemplateArgumentNode expr.ext (Just name) <$> expression
         Nothing -> pure $ TemplateArgumentNode
           expr.ext{error=[i|Invalid template argument name: #{expr}|]}
           Nothing
           expr

  templateArgumentList <- rule do
    between (token Lexer.LessThan) (token Lexer.GreaterThan) $
      templateArgument `sepBy1` token Lexer.Comma

  templateParameterList <- rule do
    between (token Lexer.LessThan) (token Lexer.GreaterThan) do
      params <- templateParameter `sepBy1` token Lexer.Comma
      optional (token Lexer.Comma)
      return params

  templateParameter <- rule do
    id_ <- identifier
    constraint <- optional do
      token Lexer.ExtendsKeyword
      return mixedParameterConstraint
    defaultExp <- optional $ token Lexer.Equals >> expression
    return $ TemplateParameterDeclarationNode id_.ext id_ constraint defaultExp

  mixedParameterConstraint <- rule do
    optional (token Lexer.Bar)
    nodes <- valueOfOrIntersectionOrHigherExpression `sepBy1` token Lexer.Bar
    return $ case length nodes of
      1 -> head nodes
      _ -> UnionE $ UnionExpressionNode meta nodes

  valueOfOrIntersectionOrHigherExpression <- rule
    (   ValueOfE <$> do
          location <- locatedToken Lexer.ValueOfKeyword
          ValueOfExpressionNode meta{location} <$> expression

    <|> between (token Lexer.OpenParen) (token Lexer.CloseParen) mixedParameterConstraint
    <|> intersectionOrHigherExpression
    )

  let expression = unionOrHigherExpression

  unionOrHigherExpression <- rule do
    optional (token Lexer.Bar)
    nodes <- intersectionOrHigherExpression `sepBy1` token Lexer.Bar
    return $ case length nodes of
      1 -> head nodes
      _ -> UnionE $ UnionExpressionNode meta nodes

  intersectionOrHigherExpression <- rule do
    optional (token Lexer.Ampersand)
    nodes <- arrayOrHigherExpression `sepBy1` token Lexer.Ampersand
    return $ case length nodes of
      1 -> head nodes
      _ -> IntersectionE $ IntersectionExpressionNode meta nodes

  arrayOrHigherExpression <- rule do
    expr <- primaryExpression
    arrs <- many do
      token Lexer.OpenBracket
      token Lexer.CloseBracket
    return $ foldl (\e _ -> ArrayE $ ArrayExpressionNode e.ext e) expr arrs

  primaryExpression <- rule
    (   TypeOfE <$> typeOfExpression
    <|> CallE <$> callExpression
    <|> TypeReferenceE <$> typeReference
    <|> StringE <$> stringLiteral
    -- TODO stringTemplateExpression
    <|> BooleanE <$> booleanLiteral
    <|> NumericE <$> numericLiteral
    <|> ModelE <$> modelExpression
    <|> TupleE <$> tupleExpression
    <|> between (token Lexer.OpenParen) (token Lexer.CloseParen) expression
    <|> ObjectLiteralE <$> objectLiteral
    <|> ArrayLiteralE <$> arrayLiteral
    <|> VoidE <$> voidKeyword
    <|> NeverE <$> neverKeyword
    <|> UnknownE <$> unknownKeyword
    )

  modelExpression <- rule do
    location <- locatedToken Lexer.OpenBrace
    properties <- modelProperty `sepBy` token Lexer.Semicolon
    optional (token Lexer.Semicolon)
    token Lexer.CloseBrace
    return $ ModelExpressionNode meta{location} properties

  modelProperty <- rule
    (   do
          location <- locatedToken Lexer.Ellipsis
          SpreadMP . ModelSpreadPropertyNode meta{location} <$> typeReference

    <|> do
          id_ <- identifier
          optional_ <- isJust <$> optional (token Lexer.Question)
          token Lexer.Colon
          value <- expression
          defaultExp <- optional do
            token Lexer.Equals
            expression
          -- TODO decorators
          return $ DirectMP (ModelPropertyNode id_.ext id_ [] defaultExp optional_ value)
    )

  tupleExpression <- rule do
    location <- locatedToken Lexer.OpenBracket
    exprs <- expression `sepBy` token Lexer.Comma
    optional (token Lexer.Comma)
    token Lexer.CloseBracket
    return $ TupleExpressionNode meta{location} exprs

  objectLiteral <- rule do
    location <- locatedToken Lexer.HashBrace
    properties <- objectLiteralProperty `sepBy` token Lexer.Comma
    optional (token Lexer.Comma)
    token Lexer.CloseBrace
    return $ ObjectLiteralNode meta{location} properties

  objectLiteralProperty <- rule
    (   do
          location <- locatedToken Lexer.Ellipsis
          SpreadOLP . ObjectLiteralSpreadPropertyNode meta{location} <$> typeReference

    <|> do
          id_ <- identifier
          token Lexer.Colon
          DirectOLP . ObjectLiteralPropertyNode id_.ext id_ <$> expression
    )

  arrayLiteral <- rule do
    location <- locatedToken Lexer.HashBracket
    exprs <- expression `sepBy` token Lexer.Comma
    optional (token Lexer.Comma)
    token Lexer.CloseBracket
    return $ ArrayLiteralNode meta{location} exprs

  statement <- rule
    (   ImportS <$> importStatement
    <|> ModelS <$> modelStatement
    <|> ScalarS <$> scalarStatement
    <|> NamespaceS <$> namespaceStatement
    <|> InterfaceS <$> interfaceStatement
    <|> UnionS <$> unionStatement
    <|> OperationS <$> operationStatement
    <|> EnumS <$> enumStatement
    <|> AliasS <$> aliasStatement
    <|> ConstS <$> constStatement
    <|> UsingS <$> usingStatement
    <|> EmptyS <$> emptyStatement
    <|> DecoratorS <$> decoratorDeclaration
    <|> FunctionS <$> functionDeclaration
    )

  statementList <- rule $ between (token Lexer.OpenBrace) (token Lexer.CloseBrace) $ many statement

  importStatement <- rule do
    location <- locatedToken Lexer.ImportKeyword
    path <- stringLiteral
    token Lexer.Semicolon
    return $ ImportStatementNode meta{location} path

  modelStatement <- rule do
    location <- locatedToken Lexer.ModelKeyword
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    base <- optional modelBase
    modelExp <- case base of
      Just (IsMB _) -> do
        semi <- optional (token Lexer.Semicolon)
        case semi of
          Just _ -> return Nothing
          _ -> Just <$> modelExpression
      _ -> Just <$> modelExpression
    let properties = maybe [] (\e -> e.properties) modelExp
    -- TODO decorators
    return $ ModelStatementNode meta{location} id_ templateParameters [] base properties

  modelBase <- rule
    (   ExtendsMB <$> (token Lexer.ExtendsKeyword *> expression)
    <|> IsMB <$> (token Lexer.IsKeyword *> expression)
    )

  scalarStatement <- rule do
    location <- locatedToken Lexer.ScalarKeyword
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    extends <- optional do
      token Lexer.ExtendsKeyword
      typeReference
    ScalarStatementNode meta{location} id_ templateParameters [] extends <$> scalarMembers

  scalarMembers <- rule
    (   token Lexer.Semicolon $> []
    <|> between (token Lexer.OpenBrace) (token Lexer.CloseBrace) do
          members <- scalarMember `sepBy` token Lexer.Semicolon
          optional (token Lexer.Semicolon)
          return members
    )

  scalarMember <- rule do
    location <- locatedToken Lexer.InitKeyword
    id_ <- identifier
    ScalarConstructorNode meta{location} id_ <$> functionParameterList

  namespaceStatement <- rule do
    location <- locatedToken Lexer.NamespaceKeyword
    ids <- identifier `sepBy1` token Lexer.Dot
    let innerId = NonEmpty.last ids
    let outerIds = NonEmpty.init ids
    statements <- option [] statementList
    let inner = NamespaceStatementNode meta{location} innerId [] statements
    return $ foldr (\id_ ns -> NamespaceStatementNode meta{location} id_ [] [NamespaceS ns]) inner outerIds

  interfaceStatement <- rule do
    location <- locatedToken Lexer.InterfaceKeyword
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    extends <- typeReference `sepBy` token Lexer.Comma
    operations <- between (token Lexer.OpenBrace) (token Lexer.CloseBrace) do
      ops <- interfaceOperation `sepBy` token Lexer.Semicolon
      optional (token Lexer.Semicolon)
      return ops
    return $ InterfaceStatementNode meta{location} id_ templateParameters [] extends operations

  interfaceOperation <- rule $
    optional (token Lexer.OpKeyword) *> operation

  unionStatement <- rule do
    location <- locatedToken Lexer.UnionKeyword
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    options <- between (token Lexer.OpenBrace) (token Lexer.CloseBrace) do
      variants <- unionVariant `sepBy` (token Lexer.Semicolon <|> token Lexer.Comma)
      optional (token Lexer.Semicolon <|> token Lexer.Comma)
      return variants
    return $ UnionStatementNode meta{location} id_ templateParameters [] options

  unionVariant <- rule
    (   do id_ <- identifier
           token Lexer.Colon
           UnionVariantNode id_.ext (Just id_) [] <$> expression

    <|> do expr <- expression
           return $ UnionVariantNode expr.ext Nothing [] expr
    )

  operationStatement <- rule $
    token Lexer.OpKeyword *> operation <* token Lexer.Semicolon

  operation <- rule do
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    OperationStatementNode id_.ext id_ templateParameters [] <$> operationSignature

  operationSignature <- rule
    (   DeclarationOS <$> do
          location <- locatedToken Lexer.OpenParen
          parameters <- ModelExpressionNode meta{location} <$> modelProperty `sepBy` token Lexer.Comma
          optional (token Lexer.Comma)
          token Lexer.CloseParen
          token Lexer.Colon
          OperationSignatureDeclarationNode meta{location} parameters <$> expression

    <|> ReferenceOS <$> do
          location <- locatedToken Lexer.IsKeyword
          OperationSignatureReferenceNode meta{location} <$> typeReference
    )

  enumStatement <- rule do
    location <- locatedToken Lexer.EnumKeyword
    id_ <- identifier
    members <- between (token Lexer.OpenBrace) (token Lexer.CloseBrace) do
      members_ <- enumMember `sepBy` (token Lexer.Semicolon <|> token Lexer.Comma)
      optional (token Lexer.Semicolon <|> token Lexer.Comma)
      return members_
    return $ EnumStatementNode meta{location} id_ [] members

  enumMember <- rule
    (   SpreadEM <$> do
          location <- locatedToken Lexer.Ellipsis
          EnumSpreadMemberNode meta{location} <$> typeReference

    <|> DirectEM <$> do
          id_ <- identifier
          value <- optional do
            token Lexer.Colon
            (Left <$> stringLiteral) <|> (Right <$> numericLiteral)
          return $ EnumMemberNode id_.ext id_ [] value
    )

  aliasStatement <- rule do
    location <- locatedToken Lexer.AliasKeyword
    id_ <- identifier
    templateParameters <- option [] (toList <$> templateParameterList)
    token Lexer.Equals
    value <- expression
    token Lexer.Semicolon
    return $ AliasStatementNode meta{location} id_ templateParameters value

  constStatement <- rule do
    location <- locatedToken Lexer.ConstKeyword
    id_ <- identifier
    typ <- optional $ token Lexer.Colon *> expression
    token Lexer.Equals
    value <- expression
    token Lexer.Semicolon
    return $ ConstStatementNode meta{location} id_ typ value

  usingStatement <- rule do
    location <- locatedToken Lexer.UsingKeyword
    name <- memberExpression
    token Lexer.Semicolon
    return $ UsingStatementNode meta{location} name

  emptyStatement <- rule do
    location <- locatedToken Lexer.Semicolon
    return $ EmptyStatementNode meta{location}

  modifier <- rule $ ExternM <$> externKeyword

  decoratorDeclaration <- rule do
    modifiers <- many modifier
    location <- locatedToken Lexer.DecKeyword
    id_ <- identifier
    (target : parameters) <- functionParameterList
    token Lexer.Semicolon
    return $ DecoratorDeclarationStatementNode meta{location} id_ modifiers target parameters

  functionDeclaration <- rule do
    modifiers <- many modifier
    location <- locatedToken Lexer.FnKeyword
    id_ <- identifier
    parameters <- functionParameterList
    returnType <- optional $ token Lexer.Colon >> expression
    token Lexer.Semicolon
    return $ FunctionDeclarationStatementNode meta{location} id_ modifiers parameters returnType

  identifier <- rule do
    ~(location, name) <- locatedIdent
    return $ IdentifierNode meta{location} name

  booleanLiteral <- rule do
    ~(location, value) <- locatedBool
    return $ BooleanLiteralNode meta{location} value

  numericLiteral <- rule do
    ~(location, value) <- locatedNumber
    return $ NumericLiteralNode meta{location} value

  stringLiteral <- rule do
    ~(location, text) <- locatedString
    return $ StringLiteralNode meta{location} text

  externKeyword <- rule do
    location <- locatedToken Lexer.ExternKeyword
    return $ ExternKeywordNode meta{location}

  voidKeyword <- rule do
    location <- locatedToken Lexer.VoidKeyword
    return $ VoidKeywordNode meta{location}

  neverKeyword <- rule do
    location <- locatedToken Lexer.NeverKeyword
    return $ NeverKeywordNode meta{location}

  unknownKeyword <- rule do
    location <- locatedToken Lexer.UnknownKeyword
    return $ UnknownKeywordNode meta{location}

  return node

-- | Parse a complete expression
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError (Node Meta)
parse name code = do
    tokens <- Lexer.lex name code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let offset =
                    case unconsumed of
                        []                -> Offset (Text.length code)
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (ParsingFailed (Location{..}))

        (result : _, _) -> do
            return result
