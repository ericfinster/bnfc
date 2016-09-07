{-
    BNF Converter: Abstract syntax Generator
    Copyright (C) 2004  Author:  Markus Forberg
    Copyright (C) 2016  Author:  Eric Finster

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Backend.Scala.CFtoAbstract (cf2Abstract) where

import BNFC.CF
-- import BNFC.Utils((+++))
-- import BNFC.Backend.Haskell.Utils (catToType, catvars)
import Text.PrettyPrint

-- produce the scala abstract syntax
cf2Abstract :: CF -> String
cf2Abstract cf = render (vcat (text "package bnfc" : punctuate (text "\n") (map (genTrait) (cf2data cf))))

-- Derived from the similar function in Haskell.  Probably
-- needs to be looked at more closely
catToType :: Cat -> Doc
catToType InternalCat = error "Internal category"
catToType (Cat c) = text c <> text "T"
catToType (CoercCat c _) = text c <> text "T" 
catToType (TokenCat c) = text (rewriteToken c)
catToType (ListCat c) = text "List[" <> (catToType c) <> text "]"

-- The last case is a bit simplistic.  You should somehow generate
-- "type s = String" aliases so that you can preserve the naming....
rewriteToken :: String -> String
rewriteToken "Integer" = "Int"
rewriteToken "Double" = "Double"
rewriteToken "Char" = "Char"
rewriteToken s = "String"  

genTrait :: Data -> Doc
genTrait (cat, rules) =
  let ct = catToType cat
      st = text "sealed trait" <+> ct
      caseclass (fun,rules) = text ("case class " ++ fun) <>
        parens (hsep (punctuate (text ",") (map genArg (zip [0..] rules)))) <+>
        "extends" <+> ct
      genArg (i, h) = text ("arg" ++ show i) <+> text ":" <+> catToType h
  in st $+$ vcat (map caseclass rules)

