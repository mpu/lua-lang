module Language.Lua.Parse (lexp, lstat, lblock) where

import Language.Lua.Syntax
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

luadef = emptyDef { P.commentStart = "--[["
                  , P.commentEnd = "]]"
                  , P.commentLine = "--"
                  , P.nestedComments = True
                  , P.identStart = lower <|> upper
                  , P.identLetter = lower <|> upper <|> digit <|> char '_'
                  , P.reservedNames =
                      [ "if", "else", "elseif", "end", "do", "for"
                      , "function", "return", "true", "false", "nil"
                      , "local" ]
                  }

lexer = P.makeTokenParser luadef

ident = (Name <$> P.identifier lexer) <|>
        (AName <$> lexeme (char '`' *> P.identifier lexer))
num = fromInteger <$> P.decimal lexer
lexeme = P.lexeme lexer
sym = try . P.symbol lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
semi = option "" (P.semi lexer)
stringLit = P.stringLiteral lexer

binop = (sym "and" >> return And) <|> (sym "or" >> return Or) <|> (sym "==" >> return Eq)
unop = sym "not" >> return Not

args = parens (lexp `sepBy` sym ",")

lexp = EUnOp <$> unop <*> lexp <|> binexp
    where binexp = do f <- lexeme factor
                      op <- optionMaybe binop
                      case op of
                          Just op -> EBinOp op f <$> lexp
                          Nothing -> return f
          factor = (sym "nil" >> return ENil)
                   <|> (sym "function" >> funbody)
                   <|> EAnti <$> (char '$' >> ident)
                   <|> EString <$> stringLit
                   <|> ENum <$> num
                   <|> EPre <$> preexp
                   <|> ETable <$> braces tfields
          funbody = do pms <- parens (lexeme ident `sepBy` sym ",")
                       bdy <- lblock
                       sym "end"
                       return $ EFun pms bdy

preexp = ((Var <$> ident <|> Parens <$> parens lexp) >>= post)
    where post e = do { char '.'; f <- ident; post (Field e f) }
               <|> do { l <- args; post (FCall e l) }
               <|> do x <- brackets (Left <$> lexp <|> Right <$> num)
                      case x of
                          Left ex -> post (Access e ex)
                          Right n -> post (Array e n)
               <|> return e

tfields = choice [tset, try tfield, texp]
              `sepBy` (P.semi lexer <|> P.comma lexer)
    where tset = assign TSet (brackets lexp)
          tfield = assign TField (lexeme ident)
          texp = TExp <$> lexp
          assign c p = c <$> p <* sym "=" <*> lexp

lstat = choice [ dostat , ifstat, try fundec
               , choice [ret, lassign, try gassign, Call <$> try preexp] <* semi ]
    where dostat = between (sym "do") (sym "end") lblock >>= return . Do
          ifstat = between (sym "if") (sym "end") (conds [])
          conds l = do e <- lexp
                       sym "then"
                       b <- lblock
                       let m = (e, b) : l
                       (sym "elseif" >> conds m)
                         <|> If (reverse m) <$> optionMaybe (sym "else" >> lblock)
          fundec = do scope <- optionMaybe (sym "local")
                      sym "function"
                      f <- ident
                      p <- parens (ident `sepBy` sym ",")
                      b <- lblock
                      sym "end"
                      case scope of
                          Nothing -> return $ Assign [Var f] [EFun p b]
                          Just _ -> return $ BindFun f p b
          ret = sym "return" >> Ret <$> lexp
          lassign = do sym "local"
                       names <- ident `sepBy1` sym ","
                       vals <- (sym "=" >> lexp `sepBy1` sym ",") <|> return []
                       return $ Bind names vals
          gassign = do lvas <- preexp `sepBy1` sym ","
                       sym "="
                       vals <- lexp `sepBy1` sym ","
                       return $ Assign lvas vals

lblock = Block <$> many lstat
