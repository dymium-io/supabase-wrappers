module Types.Lang where

import           RIO

import           Text.Read
import qualified Text.Read.Lex as L

data Lang
  = Haskell
  | Python
  | GoLang
  | JS
  | TS
    deriving(Eq,Enum,Bounded)

instance Read Lang where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "haskell" -> return Haskell
           "python"  -> return Python
           "golang"  -> return GoLang
           "js"      -> return JS
           "ts"      -> return TS
           _         -> pfail
    )

instance Show Lang where
  show Haskell = "haskell"
  show Python  = "python"
  show GoLang  = "golang"
  show JS      = "js"
  show TS      = "ts"
