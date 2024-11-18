{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.TypeSpec.Syntax.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)

data Node p
  = ExpressionN (Expression p)
  | StatementN (Statement p)
  deriving (Eq, Show)

data Expression p
  = ArrayE (ArrayExpressionNode p)
  | MemberE (MemberExpression p)
  | ModelE (ModelExpressionNode p)
  | ObjectLiteralE (ObjectLiteralNode p)
  | ArrayLiteralE (ArrayLiteralNode p)
  | TupleE (TupleExpressionNode p)
  | UnionE (UnionExpressionNode p)
  | IntersectionE (IntersectionExpressionNode p)
  | TypeReferenceE (TypeReferenceNode p)
  | ValueOfE (ValueOfExpressionNode p)
  | TypeOfE (TypeOfExpressionNode p)
  | CallE (CallExpressionNode p)
  | StringE (StringLiteralNode p)
  | NumericE (NumericLiteralNode p)
  | BooleanE (BooleanLiteralNode p)
  | StringTemplateE (StringTemplateExpressionNode p)
  | VoidE (VoidKeywordNode p)
  | NeverE (NeverKeywordNode p)
  | UnknownE (UnknownKeywordNode p)
  deriving (Eq, Show)

data ArrayExpressionNode p = ArrayExpressionNode
  { ext :: p
  , elementType :: Expression p
  }
  deriving (Eq, Show)

data MemberExpression p
  = MemberME (MemberNode p)
  | MetaMemberME (MetaMemberNode p)
  deriving (Eq, Show)

data MemberNode p = MemberNode
  { ext :: p
  , ids :: NonEmpty (IdentifierNode p)
  }
  deriving (Eq, Show)

data MetaMemberNode p = MetaMemberNode
  { ext :: p
  , base :: MemberNode p
  , member :: IdentifierNode p
  }
  deriving (Eq, Show)

data ModelExpressionNode p = ModelExpressionNode
  { ext :: p
  , properties :: [ModelProperty p]
  }
  deriving (Eq, Show)

data ObjectLiteralNode p = ObjectLiteralNode
  { ext :: p
  , properties :: [ObjectLiteralProperty p]
  }
  deriving (Eq, Show)

data ArrayLiteralNode p = ArrayLiteralNode
  { ext :: p
  , values :: [Expression p]
  }
  deriving (Eq, Show)

data TupleExpressionNode p = TupleExpressionNode
  { ext :: p
  , values :: [Expression p]
  }
  deriving (Eq, Show)

data UnionExpressionNode p = UnionExpressionNode
  { ext :: p
  , options :: NonEmpty (Expression p)
  }
  deriving (Eq, Show)

data IntersectionExpressionNode p = IntersectionExpressionNode
  { ext :: p
  , options :: NonEmpty (Expression p)
  }
  deriving (Eq, Show)

data TypeReferenceNode p = TypeReferenceNode
  { ext :: p
  , target :: MemberExpression p
  , arguments :: [TemplateArgumentNode p]
  }
  deriving (Eq, Show)

data ValueOfExpressionNode p = ValueOfExpressionNode
  { ext :: p
  , target :: Expression p
  }
  deriving (Eq, Show)

data TypeOfExpressionNode p = TypeOfExpressionNode
  { ext :: p
  , target :: Expression p
  }
  deriving (Eq, Show)

data CallExpressionNode p = CallExpressionNode
  { ext :: p
  , target :: MemberExpression p
  , arguments :: [Expression p]
  }
  deriving (Eq, Show)

data StringTemplateExpressionNode p = StringTemplateExpressionNode
  { ext :: p
  , head :: StringTemplateHeadNode p
  , spans :: [StringTemplateSpanNode p]
  }
  deriving (Eq, Show)

newtype VoidKeywordNode p = VoidKeywordNode
  { ext :: p
  }
  deriving (Eq, Show)

newtype NeverKeywordNode p = NeverKeywordNode
  { ext :: p
  }
  deriving (Eq, Show)

newtype UnknownKeywordNode p = UnknownKeywordNode
  { ext :: p
  }
  deriving (Eq, Show)

data StringLiteralNode p = StringLiteralNode
  { ext :: p
  , value :: Text
  }
  deriving (Eq, Show)

data NumericLiteralNode p = NumericLiteralNode
  { ext :: p
  , value :: Scientific
  }
  deriving (Eq, Show)

data BooleanLiteralNode p = BooleanLiteralNode
  { ext :: p
  , value :: Bool
  }
  deriving (Eq, Show)

data StringTemplateHeadNode p = StringTemplateHeadNode
  { ext :: p
  , value :: Text
  }
  deriving (Eq, Show)

data StringTemplateMiddleNode p = StringTemplateMiddleNode
  { ext :: p
  , value :: Text
  }
  deriving (Eq, Show)

data StringTemplateTailNode p = StringTemplateTailNode
  { ext :: p
  , value :: Text
  }
  deriving (Eq, Show)

data DecoratorExpressionNode p = DecoratorExpressionNode
  { ext :: p
  , target :: MemberExpression p
  , arguments :: [Expression p]
  }
  deriving (Eq, Show)

data DirectiveExpressionNode p = DirectiveExpressionNode
  { ext :: p
  , target :: IdentifierNode p
  , arguments :: [DirectiveArgument p]
  }
  deriving (Eq, Show)

data DocNode p = DocNode
  { ext :: p
  , content :: [DocContent p]
  , tags :: [DocTag p]
  }
  deriving (Eq, Show)

data DocTextNode p = DocTextNode
  { ext :: p
  , text :: Text
  }
  deriving (Eq, Show)

data EnumMember p
  = DirectEM (EnumMemberNode p)
  | SpreadEM (EnumSpreadMemberNode p)
  deriving (Eq, Show)

data EnumMemberNode p = EnumMemberNode
  { ext :: p
  , id :: IdentifierNode p
  , decorators :: [DecoratorExpressionNode p]
  , value :: Maybe (Either (StringLiteralNode p) (NumericLiteralNode p))
  }
  deriving (Eq, Show)

data EnumSpreadMemberNode p = EnumSpreadMemberNode
  { ext :: p
  , target :: TypeReferenceNode p
  }
  deriving (Eq, Show)

data FunctionParameterNode p = FunctionParameterNode
  { ext :: p
  , id :: IdentifierNode p
  , optional :: Bool
  , rest :: Bool
  , typ :: Maybe (Expression p)
  }
  deriving (Eq, Show)

data ModelProperty p
  = DirectMP (ModelPropertyNode p)
  | SpreadMP (ModelSpreadPropertyNode p)
  deriving (Eq, Show)

data ModelPropertyNode p = ModelPropertyNode
  { ext :: p
  , id :: IdentifierNode p
  , decorators :: [DecoratorExpressionNode p]
  , defaultExp :: Maybe (Expression p)
  , optional :: Bool
  , value :: Expression p
  }
  deriving (Eq, Show)

data ModelSpreadPropertyNode p = ModelSpreadPropertyNode
  { ext :: p
  , target :: TypeReferenceNode p
  }
  deriving (Eq, Show)

data ObjectLiteralProperty p
  = DirectOLP (ObjectLiteralPropertyNode p)
  | SpreadOLP (ObjectLiteralSpreadPropertyNode p)
  deriving (Eq, Show)

data ObjectLiteralPropertyNode p = ObjectLiteralPropertyNode
  { ext :: p
  , id :: IdentifierNode p
  , value :: Expression p
  }
  deriving (Eq, Show)

data ObjectLiteralSpreadPropertyNode p = ObjectLiteralSpreadPropertyNode
  { ext :: p
  , target :: TypeReferenceNode p
  }
  deriving (Eq, Show)

data OperationSignatureDeclarationNode p = OperationSignatureDeclarationNode
  { ext :: p
  , parameters :: ModelExpressionNode p
  , returnType :: Expression p
  }
  deriving (Eq, Show)

data OperationSignatureReferenceNode p = OperationSignatureReferenceNode
  { ext :: p
  , base :: TypeReferenceNode p
  }
  deriving (Eq, Show)

data ScalarConstructorNode p = ScalarConstructorNode
  { ext :: p
  , id :: IdentifierNode p
  , parameters :: [FunctionParameterNode p]
  }
  deriving (Eq, Show)

data ScriptNode p = ScriptNode
  { ext :: p
  , file :: SourceFile
  , comments :: [Comment p]
  , statements :: [Statement p]
  }
  deriving (Eq, Show)

data StringTemplateSpanNode p = StringTemplateSpanNode
  { ext :: p
  , literal :: Either (StringTemplateMiddleNode p) (StringTemplateTailNode p)
  , expression :: Expression p
  }
  deriving (Eq, Show)

data TemplateArgumentNode p = TemplateArgumentNode
  { ext :: p
  , name :: Maybe (IdentifierNode p)
  , argument :: Expression p
  }
  deriving (Eq, Show)

data UnionVariantNode p = UnionVariantNode
  { ext :: p
  , id :: Maybe (IdentifierNode p)
  , decorators :: [DecoratorExpressionNode p]
  , value :: Expression p
  }
  deriving (Eq, Show)

data Statement p
  = ImportS (ImportStatementNode p)
  | ModelS (ModelStatementNode p)
  | ScalarS (ScalarStatementNode p)
  | NamespaceS (NamespaceStatementNode p)
  | InterfaceS (InterfaceStatementNode p)
  | UnionS (UnionStatementNode p)
  | UsingS (UsingStatementNode p)
  | EnumS (EnumStatementNode p)
  | AliasS (AliasStatementNode p)
  | OperationS (OperationStatementNode p)
  | DecoratorS (DecoratorDeclarationStatementNode p)
  | FunctionS (FunctionDeclarationStatementNode p)
  | AugmentDecoratorS (AugmentDecoratorStatementNode p)
  | ConstS (ConstStatementNode p)
  | CallS (CallExpressionNode p)
  | EmptyS (EmptyStatementNode p)
  | InvalidS (InvalidStatementNode p)
  deriving (Eq, Show)

data ImportStatementNode p = ImportStatementNode
  { ext :: p
  , path :: StringLiteralNode p
  }
  deriving (Eq, Show)

data ModelStatementNode p = ModelStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , decorators :: [DecoratorExpressionNode p]
  , base :: Maybe (ModelBase p)
  , properties :: [ModelProperty p]
  }
  deriving (Eq, Show)

data ModelBase p
  = IsMB (Expression p)
  | ExtendsMB (Expression p)
  deriving (Eq, Show)

data ScalarStatementNode p = ScalarStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , decorators :: [DecoratorExpressionNode p]
  , extends :: Maybe (TypeReferenceNode p)
  , members :: [ScalarConstructorNode p]
  }
  deriving (Eq, Show)

data NamespaceStatementNode p = NamespaceStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , decorators :: [DecoratorExpressionNode p]
  , statements :: [Statement p]
  }
  deriving (Eq, Show)

data InterfaceStatementNode p = InterfaceStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , decorators :: [DecoratorExpressionNode p]
  , extends :: [TypeReferenceNode p]
  , operations :: [OperationStatementNode p]
  }
  deriving (Eq, Show)

data UnionStatementNode p = UnionStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , decorators :: [DecoratorExpressionNode p]
  , options :: [UnionVariantNode p]
  }
  deriving (Eq, Show)

data UsingStatementNode p = UsingStatementNode
  { ext :: p
  , name :: MemberExpression p
  }
  deriving (Eq, Show)

data EnumStatementNode p = EnumStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , decorators :: [DecoratorExpressionNode p]
  , members :: [EnumMember p]
  }
  deriving (Eq, Show)

data AliasStatementNode p = AliasStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , value :: Expression p
  }
  deriving (Eq, Show)

data OperationStatementNode p = OperationStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , templateParameters :: [TemplateParameterDeclarationNode p]
  , decorators :: [DecoratorExpressionNode p]
  , signature :: OperationSignature p
  }
  deriving (Eq, Show)

data DecoratorDeclarationStatementNode p = DecoratorDeclarationStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , modifiers :: [Modifier p]
  , target :: FunctionParameterNode p
  , parameters :: [FunctionParameterNode p]
  }
  deriving (Eq, Show)

data FunctionDeclarationStatementNode p = FunctionDeclarationStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , modifiers :: [Modifier p]
  , parameters :: [FunctionParameterNode p]
  , retunType :: Maybe (Expression p)
  }
  deriving (Eq, Show)

data AugmentDecoratorStatementNode p = AugmentDecoratorStatementNode
  { ext :: p
  , target :: MemberExpression p
  , targetType :: TypeReferenceNode p
  }
  deriving (Eq, Show)

data ConstStatementNode p = ConstStatementNode
  { ext :: p
  , id :: IdentifierNode p
  , typ :: Maybe (Expression p)
  , value :: Expression p
  }
  deriving (Eq, Show)

newtype EmptyStatementNode p = EmptyStatementNode
  { ext :: p
  }
  deriving (Eq, Show)

data InvalidStatementNode p = InvalidStatementNode
  { ext :: p
  , decorators :: [DecoratorExpressionNode p]
  }
  deriving (Eq, Show)

data DocTag p
  = ReturnsDT (DocReturnsTagNode p)
  | ErrorsDT (DocErrorsTagNode p)
  | ParamDT (DocParamTagNode p)
  | PropDT (DocPropTagNode p)
  | TemplateDT (DocTemplateTagNode p)
  | UnknownDT (DocUnknownTagNode p)
  deriving (Eq, Show)

data DocReturnsTagNode p = DocReturnsTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  }
  deriving (Eq, Show)

data DocErrorsTagNode p = DocErrorsTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  }
  deriving (Eq, Show)

data DocParamTagNode p = DocParamTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  , param :: IdentifierNode p
  }
  deriving (Eq, Show)

data DocPropTagNode p = DocPropTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  , prop :: IdentifierNode p
  }
  deriving (Eq, Show)

data DocTemplateTagNode p = DocTemplateTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  , param :: IdentifierNode p
  }
  deriving (Eq, Show)

data DocUnknownTagNode p = DocUnknownTagNode
  { ext :: p
  , tag :: IdentifierNode p
  , content :: [DocTextNode p]
  }
  deriving (Eq, Show)

data DirectiveArgument p
  = StringDA (StringLiteralNode p)
  | IdentifierDA (IdentifierNode p)
  deriving (Eq, Show)

newtype DocContent p = TextDC (DocTextNode p)
  deriving (Eq, Show)

data OperationSignature p
  = DeclarationOS (OperationSignatureDeclarationNode p)
  | ReferenceOS (OperationSignatureReferenceNode p)
  deriving (Eq, Show)

data IdentifierNode p = IdentifierNode
  { ext :: p
  , name :: Text
  }
  deriving (Eq, Show)

newtype Modifier p = ExternM (ExternKeywordNode p)
  deriving (Eq, Show)

newtype ExternKeywordNode p = ExternKeywordNode
  { ext :: p
  }
  deriving (Eq, Show)

data Comment p
  = LineComment p Text
  | BlockComment p Text
  deriving (Eq, Show)

data Selector = MemberSelector | MetaSelector
  deriving (Eq, Show)

data SourceFile = SourceFile
  { path :: Text
  , text :: Text
  }
  deriving (Eq, Show)

data TemplateParameterDeclarationNode p = TemplateParameterDeclarationNode
  { ext :: p
  , id :: IdentifierNode p
  , constraint :: Maybe (Expression p)
  , defaultExp :: Maybe (Expression p)
  }
  deriving (Eq, Show)
