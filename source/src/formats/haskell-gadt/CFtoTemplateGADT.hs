{-
    BNF Converter: GADT Template Generator
    Copyright (C) 2004-2005  Author:  Markus Forberg, Bj�rn Bringert

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


module CFtoTemplateGADT (
		    cf2Template
                    ) where

import CF
import Utils((+++))
import Data.List (delete,groupBy)

import HaskellGADTCommon

type ModuleName = String

cf2Template :: ModuleName -> ModuleName -> ModuleName -> CF -> String
cf2Template skelName absName errName cf = unlines $
  [
  "module "++ skelName ++ " where",
  "",
  "-- Haskell module generated by the BNF converter",
  "",
  "import " ++ absName,
  "import " ++ errName,
  "type Result = Err String\n",
  "failure :: Show a => a -> Result",
  "failure x = Bad $ \"Undefined case: \" ++ show x",
  "",
  "transTree :: Tree c -> Result",
  "transTree t = case t of"]
  ++ map prConsCase (cf2cons cf)
  ++ [""]
  ++ concatMap ((++[""]) . uncurry prCatTrans) (catCons cf)

prConsCase :: Constructor -> String
prConsCase c =
  "  " ++ consFun c +++ unwords (map snd (consVars c)) +++ "-> failure t"

catCons :: CF -> [(Cat,[Constructor])]
catCons cf = [ (consCat (head cs),cs) | cs <- groupBy catEq $ cf2cons cf]

catEq :: Constructor -> Constructor -> Bool
catEq c1 c2 = consCat c1 == consCat c2

prCatTrans :: Cat -> [Constructor] -> [String]
prCatTrans cat cs = ["trans" ++ cat +++ "::" +++ cat +++ "-> Result",
		     "trans" ++ cat +++ "t = case t of"]
		    ++ map prConsCase cs