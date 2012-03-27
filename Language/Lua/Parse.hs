module Language.Lua.Parse
    ( lexp
    , lstat
    ) where

import Language.Lua.Syntax
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))

idstart, idchar :: Parser Char
semi :: Parser ()

idstart = lower <|> upper
idchar = idstart <|> digit <|> char '_'

ident = (:) <$> idstart <*> many idchar >>= return . Name
num = many1 (digit :: Parser Char) >>= return . (read :: String -> Int)
semi = option () (sym ";" >> return ())

lexeme p = do { x <- p; spaces; return x }
sym = lexeme . string
parens = between (sym "(") (sym ")")
brackets = between (sym "[") (sym "]")

binop = (sym "and" >> return And) <|> (sym "or" >> return Or) <|> (sym "==" >> return Eq)
unop = sym "not" >> return Not

fcall = FC <$> ident <*> args
args = parens (lexp `sepBy` sym ",")

lexp = EUnOp <$> try unop <*> lexp <|> binexp
    where binexp = do f <- lexeme factor
                      op <- optionMaybe binop
                      case op of
                        Just op -> EBinOp op f <$> lexp
                        Nothing -> return f
          factor = (try (sym "nil") >> return ENil)
                   <|> (try (sym "function") >> funbody)
                   <|> (EAnti <$> (char '$' >> many1 idchar))
                   <|> (((EParens <$> parens lexp)
                     <|> (ECall <$> try fcall)
                     <|> (EVar <$> ident)) >>= post)
          funbody = do pms <- parens (lexeme ident `sepBy` sym ",")
                       bdy <- statl
                       sym "end"
                       return $ EFun pms bdy

post f = do { char '.'; fi <- ident; post (f . Field fi) }
     <|> do { l <- args; post (f . FCall l) }
     <|> do e <- brackets (Left <$> lexp <|> Right <$> num)
            case e of
              Left e -> post (f . Access e)
              Right n -> post (f . Array n)
     <|> return (f Nil)

lstat = dostat <|> ifstat <|> fundec <|> ret <|> try assign <|> Call <$> try fcall
    where dostat = between (sym "do") (sym "end") statl >>= return . Do
          ifstat = between (sym "if") (sym "end") (conds [])
          conds l = do e <- lexp
                       sym "then"
                       b <- statl
                       let m = (e, b) : l
                       (try (sym "elseif") >> conds m)
                         <|> do try (sym "else")
                                el <- statl
                                return (If (reverse m) (Just el))
                         <|> return (If (reverse m) Nothing)
          fundec = do try (sym "function")
                      f <- ident
                      p <- parens (ident `sepBy` sym ",")
                      b <- statl
                      sym "end"
                      return $ Assign [B f (EFun p b)]
          ret = try (sym "return") >> Ret <$> lexp
          assign = do names <- lexeme ident `sepBy1` sym ","
                      sym "="
                      val <- lexp `sepBy1` sym ","
                      return $ Assign (zipWith B names (val ++ repeat ENil))

statl = lstat `sepBy` semi
