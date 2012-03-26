module Language.Lua.QQ
    ( luae
    ) where

import Language.Lua.Syntax
import Language.Lua.Parse
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec

parseExp :: Monad m => (String, Int, Int) -> String -> m Exp
parseExp (f, line, col) s =
    case runParser p () "" s of
      Left msg -> fail $ show msg
      Right e -> return e
  where p = do pos <- getPosition
               setPosition $ (flip setSourceName f)
                           $ (flip setSourceLine line)
                           $ (flip setSourceColumn col)
                           $ pos
               spaces
               e <- lexp
               eof
               return e

quoteLuaExp :: String -> TH.ExpQ
quoteLuaExp s = do loc <- TH.location
                   let pos = ( TH.loc_filename loc
                             , fst (TH.loc_start loc)
                             , snd (TH.loc_start loc))
                   parseExp pos s >>= dataToExpQ (const Nothing)

luae :: QuasiQuoter
luae = QuasiQuoter { quoteExp = quoteLuaExp
                   , quotePat = undefined
                   , quoteType = undefined
                   , quoteDec = undefined }
