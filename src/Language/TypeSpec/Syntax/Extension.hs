{-# LANGUAGE AllowAmbiguousTypes     #-} -- for unXRec, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]
                                         -- in module Language.Haskell.Syntax.Extension
module Language.TypeSpec.Syntax.Extension where

import Data.Type.Equality (type (~))
import Data.Data hiding ( Fixity )
import Data.Kind (Type)
import Data.Eq
import Data.Ord

data NoExt = NoExt
  deriving (Data, Eq, Ord)

-- =====================================================================
-- Type families for the Declaration extension points

type family XModelD x
type family XScalarD x
type family XInterfaceD x
type family XUnionD x
type family XNamespaceD x
type family XOperationD x
type family XTemplateParameterD x
type family XEnumD x
type family XAliasD x
type family XConstD x
type family XDecoratorD x
type family XFunctionD x
type family XXDeclaration x

-- =====================================================================
-- Type families for the Expression extension points

type family XArrayE x
type family XMemberE x
type family XModelE x
type family XObjectLiteralE x
type family XArrayLiteralE x
type family XTupleE x
type family XUnionE x
type family XIntersectionE x
type family XTypeReferenceE x
type family XValueOfE x
type family XTypeOfE x
type family XCallE x
type family XStringE x
type family XNumericE x
type family XBooleanE x
type family XStringTemplateE x
type family XVoidKeywordE x
type family XNeverKeywordE x
type family XAnyKeywordE x
type family XXExpression x

-- =====================================================================
-- Type families for the Identifier extension points
type family XIdentifier x

-- =====================================================================
-- Type families for the Node extension points

type family XScriptN x
type family XTemplateArgumentN x
type family XTemplateParameterDeclarationN x
type family XModelPropertyN x
type family XUnionVariantN x
type family XOperationStatementN x
type family XOperationSignatureDeclarationN x
type family XOperationSignatureReferenceN x
type family XEnumMemberN x
type family XEnumSpreadMemberN x
type family XModelSpreadPropertyN x
type family XDecoratorExpressionN x
type family XDirectiveExpressionN x
type family XStatementN x
type family XExpressionN x
type family XFunctionParameterN x
type family XStringTemplateSpanN x
type family XStringTemplateHeadN x
type family XStringTemplateMiddleN x
type family XStringTemplateTailN x
type family XModifierN x
type family XDocN x
type family XDocContentN x
type family XDocTagN x
type family XObjectLiteralN x
type family XObjectLiteralPropertyN x
type family XObjectLiteralSpreadPropertyN x
type family XScalarConstructorN x
type family XArrayLiteralN x
type family XXNode x

-- =====================================================================
-- Type families for the Statement extension points

type family XImportS x
type family XModelS x
type family XScalarS x
type family XNamespaceS x
type family XInterfaceS x
type family XUnionS x
type family XUsingS x
type family XEnumS x
type family XAliasS x
type family XOperationS x
type family XDecoratorDeclarationS x
type family XFunctionDeclarationS x
type family XAugmentDecoratorS x
type family XConstS x
type family XCallExpressionS x
type family XEmptyS x
type family XInvalidS x
type family XXStatement x
