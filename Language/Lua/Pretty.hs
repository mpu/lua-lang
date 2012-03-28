module Language.Lua.Pretty (Pretty(pretty)) where

import Language.Lua.Syntax
import Text.PrettyPrint.Leijen

shift = nest 4
shiftg = group . shift

instance Pretty UnOp where
    pretty Not = text "not"

instance Pretty BinOp where
    pretty Or = text "or"
    pretty And = text "and"
    pretty Eq = text "=="

instance Pretty Name where
    pretty (Name n) = pretty n

prettyPms :: Pretty a => [a] -> Doc
prettyPms = encloseSep lparen rparen (text ", ") . map pretty

instance Pretty Exp where
    pretty (EPre p) = pretty p
    pretty (EFun pms bdy) =
        group $ shift (text "function" <+> prettyPms pms <$> pretty bdy)
            <$> text "end"
    pretty (EBinOp o e1 e2) = pretty e1 <+> pretty o <+> pretty e2
    pretty (EUnOp o e) = pretty o <+> parens (pretty e)
    pretty (EAnti s) = empty
    pretty (ENil) = text "nil"

instance Pretty PreExp where
    pretty (Var n) = pretty n
    pretty (Parens e) = parens (pretty e)
    pretty (Field pre n) = pretty pre <> dot <> pretty n
    pretty (Array pre n) = pretty pre <> brackets (pretty n)
    pretty (Access pre e) = pretty pre <> brackets (pretty e)
    pretty (FCall pre l) = pretty pre <> prettyPms l

instance Pretty Stat where
    pretty (Do l) =
        group $ shift (text "do" <$> pretty l) <$> text "end"
    pretty (If l e) =
        text "if" <+> prettyConds l e <$> text "end"
      where prettyConds [] Nothing = empty
            prettyConds [] (Just l) = shiftg $ text "else" <$> pretty l
            prettyConds [(e, s)] el =
                shiftg (pretty e <+> text "then" <$> pretty s) </> prettyConds [] el
            prettyConds ((e, s) : l) el =
                shiftg (pretty e <+> text "then" <$> pretty s)
                </> text "elseif" <+> prettyConds l el
    pretty (Call fc) = pretty fc <> semi
    pretty (Ret e) = text "return" <+> pretty e <> semi
    pretty (Assign b) = pretty b <> semi

    prettyList [] = empty
    prettyList [s] = pretty s
    prettyList (s:ss) = pretty s <$> pretty ss

instance Pretty Block where
    pretty (Block b) = pretty b

instance Pretty Binding where
    pretty _ = empty

    prettyList [] = empty
    prettyList l = com ln <+> equals <+> com le
        where com :: Pretty a => [a] -> Doc
              com = encloseSep empty empty (text ", ") . map pretty
              ln = map (\(B n _) -> n) l
              le = map (\(B _ e) -> e) l
