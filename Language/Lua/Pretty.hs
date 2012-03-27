module Language.Lua.Pretty (Pretty(pretty)) where

import Language.Lua.Syntax
import Text.PrettyPrint.Leijen

shift = nest 4

instance Pretty (Name) where
    pretty (Name n) = pretty n

instance Pretty (UnOp) where
    pretty Not = text "not"

instance Pretty (BinOp) where
    pretty Or = text "or"
    pretty And = text "and"
    pretty Eq = text "=="

prettyPms :: Pretty a => [a] -> Doc
prettyPms = encloseSep (char '(') (char ')') (text ", ") . map pretty

prettyParens :: Pretty a => a -> Doc
prettyParens x = char '(' <> pretty x <> char ')'

instance Pretty (Exp) where
    pretty (EVar n post) = pretty n <> pretty post
    pretty (EFun pms bdy) =
        shift (text "function" <+> prettyPms pms </> pretty bdy)
        </> text "end"
    pretty (EParens e post) = prettyParens e <> pretty post
    pretty (ECall fc post) = pretty fc <> pretty post
    pretty (EBinOp o e1 e2) = pretty e1 <+> pretty o <+> pretty e2
    pretty (EUnOp o e) = pretty o <+> prettyParens e
    pretty (EAnti s) = empty
    pretty (ENil) = text "nil"

instance Pretty (FunCall) where
    pretty (FC n pms) = pretty n <> prettyPms pms

instance Pretty (PostExp) where
    pretty (Field n post) = char '.' <> pretty n <> pretty post
    pretty (Array n post) = char '[' <> pretty n <> char ']' <> pretty post
    pretty (Access e post) = char '[' <> pretty e <> char ']' <> pretty post
    pretty (FCall l post) = prettyPms l <> pretty post
    pretty (Nil) = empty

instance Pretty (Stat) where
    pretty (Do l) =
        shift (text "do" </> pretty l) </> text "end"
    pretty (If l e) =
        text "if" <+> prettyConds l e </> text "end"
      where prettyConds [] Nothing = empty
            prettyConds [] (Just l) = shift $ text "else" </> pretty l
            prettyConds [(e, s)] el =
                shift (pretty e <+> text "then" </> pretty s) </> prettyConds [] el
            prettyConds ((e, s) : l) el =
                shift (pretty e <+> text "then" </> pretty s)
                </> text "elseif" <+> prettyConds l el
    pretty (Call fc) = pretty fc <> char ';'
    pretty (Ret e) = text "return" <+> pretty e <> char ';'
    pretty (Assign b) = pretty b <> char ';'

    prettyList [] = empty
    prettyList [s] = pretty s
    prettyList (s:ss) = pretty s </> pretty ss

instance Pretty (Binding) where
    pretty _ = empty

    prettyList [] = empty
    prettyList l = com ln <+> char '=' <+> com le
        where com :: Pretty a => [a] -> Doc
              com = encloseSep empty empty (text ", ") . map pretty
              ln = map (\(B n _) -> n) l
              le = map (\(B _ e) -> e) l