
module Fragment where

import Haskell.Provides
import Haskell.Tweak
import Latex.Parser


data Frag = Stmt Line [String] String
          | Expr Line String
            deriving Show


parseFragments :: String -> [Frag]
parseFragments = map f . parseLatex
    where
        f (HsExpr line x) | isStmt x = f $ HsCode line x
        f (HsCode line x) = Stmt line (map fromProvides $ provides x) (tweak x)
        f (HsExpr line x) = Expr line x
