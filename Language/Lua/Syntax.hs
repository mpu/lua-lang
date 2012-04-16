module Language.Lua.Syntax
    ( UnOp(..)
    , BinOp(..)
    , Name(..)
    , Exp(..)
    , PreExp(..)
    , TAssign(..)
    , Stat(..)
    , Block(..)
    ) where

import Data.Typeable
import Data.Generics

data UnOp = Not
    deriving (Eq, Ord, Show, Typeable, Data)

data BinOp = Or | And | Eq
    deriving (Eq, Ord, Show, Typeable, Data)

data Name = Name String
          | AName String
    deriving (Eq, Ord, Show, Typeable, Data)

data Exp = EPre PreExp
         | EFun [Name] Block
         | ETable [TAssign]
         | EBinOp BinOp Exp Exp
         | EUnOp UnOp Exp
         | EAnti Name
         | EString String
         | ENil
    deriving (Eq, Ord, Show, Typeable, Data)

data PreExp = Var Name
            | Parens Exp
            | Field PreExp Name
            | Array PreExp Int
            | Access PreExp Exp
            | FCall PreExp [Exp]
    deriving (Eq, Ord, Show, Typeable, Data)

data TAssign = TField Name Exp
             | TSet Exp Exp
             | TExp Exp
    deriving (Eq, Ord, Show, Typeable, Data)

data Stat = Do Block
          | If [(Exp, Block)] (Maybe Block)
          | Call PreExp
          | Ret Exp
          | Assign [(PreExp, Exp)]
          | Bind [(Name, Exp)]
          | BindFun Name [Name] Block
    deriving (Eq, Ord, Show, Typeable, Data)

data Block = Block [Stat]
    deriving (Eq, Ord, Show, Typeable, Data)
