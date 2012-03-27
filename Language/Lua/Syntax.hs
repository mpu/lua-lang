module Language.Lua.Syntax
    ( UnOp(..)
    , BinOp(..)
    , Name(..)
    , Exp(..)
    , FunCall(..)
    , PostExp(..)
    , Stat(..)
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

data Exp = EVar Name PostExp
         | EParens Exp PostExp
         | ECall FunCall PostExp
         | EBinOp BinOp Exp Exp
         | EUnOp UnOp Exp
         | EAnti String
         | ENil
    deriving (Eq, Ord, Show, Typeable, Data)

data FunCall = FC Name [Exp]
    deriving (Eq, Ord, Show, Typeable, Data)

data PostExp = Field Name PostExp
             | Array Int PostExp
             | Access Exp PostExp
             | FCall [Exp] PostExp
             | Nil
    deriving (Eq, Ord, Show, Typeable, Data)

data Stat = Do [Stat]
          | If [(Exp, [Stat])] (Maybe [Stat])
          | FunDecl Name [Name] [Stat]
          | Call FunCall
          | Assign [Binding]
    deriving (Eq, Ord, Show, Typeable, Data)

data Binding = B Name Exp
             | L Name Exp
    deriving (Eq, Ord, Show, Typeable, Data)
