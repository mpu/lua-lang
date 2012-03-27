module Language.Lua.QQ
    ( luae
    , luas
    ) where

import Data.Generics
import Language.Lua.Syntax
import Language.Lua.Parse
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (parse)
import Text.Parsec.String

parse :: Monad m => Parser a -> (String, Int, Int) -> String -> m a
parse pa (f, line, col) s =
    case runParser p () "" s of
      Left msg -> fail $ show msg
      Right x -> return x
  where p = do pos <- getPosition
               setPosition $ (flip setSourceName f)
                           $ (flip setSourceLine line)
                           $ (flip setSourceColumn col)
                           $ pos
               spaces
               x <- pa
               eof
               return x

anti :: Data a => ((String, Int, Int) -> String -> TH.Q a) -> String -> TH.ExpQ
anti f s = do loc <- TH.location
              let pos = ( TH.loc_filename loc
                        , fst (TH.loc_start loc)
                        , snd (TH.loc_start loc))
              f pos s >>= dataToExpQ (const Nothing `extQ` antiV)
    where antiV (EAnti s) = Just $ TH.varE $ TH.mkName s
          antiV _         = Nothing

mkqq :: (String -> TH.ExpQ) -> QuasiQuoter
mkqq f = QuasiQuoter { quoteExp = f
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined }

luae, luas :: QuasiQuoter

luae = mkqq $ anti $ parse lexp
luas = mkqq $ anti $ parse lstat
