module Language.Lua.Syntax
    ( UnOp(..)
    , BinOp(..)
    , Name(..)
    , Exp(..)
    , PreExp(..)
    , Stat(..)
    , Block(..)
    , Binding(..)
    ) where

import Data.Typeable
import Data.Generics

data UnOp = Not
    deriving (Eq, Ord, Show, Typeable, Data)

data BinOp = Or | And | Eq
    deriving (Eq, Ord, Show, Typeable, Data)

data Name = Name String
    deriving (Eq, Ord, Show, Typeable, Data)

data Exp = EPre PreExp
         | EFun [Name] Block
         | EBinOp BinOp Exp Exp
         | EUnOp UnOp Exp
         | EAnti Name
         | ENil
    deriving (Eq, Ord, Show, Typeable, Data)

data PreExp = Var Name
            | Parens Exp
            | Field PreExp Name
            | Array PreExp Integer
            | Access PreExp Exp
            | FCall PreExp [Exp]
    deriving (Eq, Ord, Show, Typeable, Data)

data Stat = Do Block
          | If [(Exp, Block)] (Maybe Block)
          | Call PreExp
          | Ret Exp
          | Assign [Binding]
    deriving (Eq, Ord, Show, Typeable, Data)

data Block = Block [Stat]
    deriving (Eq, Ord, Show, Typeable, Data)

data Binding = B PreExp Exp
    deriving (Eq, Ord, Show, Typeable, Data)
