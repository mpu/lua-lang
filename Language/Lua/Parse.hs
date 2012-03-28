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
                      , "function", "return", "true", "false", "nil" ]
                  }

lexer = P.makeTokenParser luadef

ident = P.identifier lexer >>= return . Name
num = P.decimal lexer
lexeme = P.lexeme lexer
sym = try . lexeme . P.symbol lexer
parens = P.parens lexer
brackets = P.brackets lexer
semi = option "" (P.semi lexer)

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
                   <|> (EAnti <$> (char '$' >> ident))
                   <|> EPre <$> preexp
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

lstat = choice [dostat, ifstat, fundec, ret, try assign, Call <$> try preexp]
    where dostat = between (sym "do") (sym "end") lblock >>= return . Do
          ifstat = between (sym "if") (sym "end") (conds [])
          conds l = do e <- lexp
                       sym "then"
                       b <- lblock
                       let m = (e, b) : l
                       (sym "elseif" >> conds m)
                         <|> do sym "else"
                                el <- lblock
                                return (If (reverse m) (Just el))
                         <|> return (If (reverse m) Nothing)
          fundec = do sym "function"
                      f <- ident
                      p <- parens (ident `sepBy` sym ",")
                      b <- lblock
                      sym "end"
                      return $ Assign [B (Var f) (EFun p b)]
          ret = sym "return" >> Ret <$> lexp
          assign = do names <- preexp `sepBy1` sym ","
                      sym "="
                      val <- lexp `sepBy1` sym ","
                      return $ Assign (zipWith B names (val ++ repeat ENil))

lblock = Block <$> lstat `sepBy` semi
