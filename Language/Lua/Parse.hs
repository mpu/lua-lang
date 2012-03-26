module Language.Lua.Parse
    ( lexp
    ) where

import Language.Lua.Syntax
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))

idstart, idchar :: Parser Char

idstart = lower <|> upper
idchar = idstart <|> digit <|> char '_'

ident = (:) <$> idstart <*> many idchar >>= return . Name
num = many1 (digit :: Parser Char) >>= return . (read :: String -> Int)

lexeme p = do { x <- p; spaces; return x }
sym = lexeme . string
parens = between (sym "(") (sym ")")
brackets = between (sym "[") (sym "]")

binop = (sym "and" >> return And) <|> (sym "or" >> return Or)
unop = sym "not" >> return Not

fcall = FC <$> ident <*> args
args = parens (lexp `sepBy` sym ",")

lexp = EUnOp <$> unop <*> lexp <|> antiexp <|> binexp
    where antiexp = do char '$'
                       EAnti <$> parens antistr
          antistr = do pre <- noparens
                       pstr <- optionMaybe (parens antistr)
                       case pstr of
                         Just l -> ((++) (pre ++ "(" ++ l ++ ")")) <$> antistr
                         Nothing -> return pre
          noparens = many (noneOf "()")
          binexp = do f <- lexeme factor
                      op <- optionMaybe binop
                      case op of
                        Just op -> EBinOp op f <$> lexp
                        Nothing -> return f
          factor = ((EParens <$> parens lexp)
                <|> (ECall <$> try fcall)
                <|> (EVar <$> ident)) >>= post

post f = do { char '.'; fi <- ident; post (f . Field fi) }
     <|> do { l <- args; post (f . FCall l) }
     <|> do e <- brackets (Left <$> lexp <|> Right <$> num)
            case e of
              Left e -> post (f . Access e)
              Right n -> post (f . Array n)
     <|> return (f Nil)
