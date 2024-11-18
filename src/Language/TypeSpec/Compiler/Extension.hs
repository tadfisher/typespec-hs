{-# LANGUAGE AllowAmbiguousTypes     #-}      -- for pprIfTc, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-} -- for IsPass; see Note [NoGhcTc]
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]
                                         -- in module Language.Haskell.Syntax.Extension

module Language.TypeSpec.Compiler.Extension where
