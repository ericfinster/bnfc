{-
    BNF Converter: Scala Bison generator
    Copyright (C) 2004  Author:  Michael Pellauer
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

{-
   **************************************************************
    BNF Scala Backend Common

    Description   : Common routines for the Scala backend

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)
                    Eric Finster (ericfinster@gmail.com)

    License       : GPL (GNU General Public License)

    Created       : 6 August, 2003

    Modified      : 6 August, 2003
                    6 September, 2016

   **************************************************************
-}

module BNFC.Backend.Scala.ScalaCommon where

import Data.List (intercalate)
import BNFC.CF

-- | slightly stronger than the NamedVariable version.
-- >>> varName (Cat "Abc")
-- "abc_"
varName :: Cat -> String
varName c = idCat (normCat c)
  where idCat (ListCat c) = "List[" ++ idCat c ++ "]"
        idCat (TokenCat c) = if (c == "Integer") then "Int" else c
        idCat c           = show c ++ "T"

-- The following is a very crude type extraction mechanism
-- so that we can synthesize the defined rules, which, in Scala,
-- must have their parameters type-annotated.

allRules :: CF -> [(Fun,[Cat])]
allRules cf = concat $ map snd (getAbstractSyntax cf)

argCats :: CF -> Fun -> Maybe [Cat]
argCats cf f = lookup f (allRules cf) 

definedRules :: CF -> [String]
definedRules cf = [ mkDef f xs e | FunDef f xs e <- cfgPragmas cf ]
  where

    doTyping f xs e = do
      typing <- checkArg cf xs e InternalCat
      sequence (map (\x -> lookup x typing >>= \c -> return $ varName c) xs)

    mkDef f xs e = case doTyping f xs e of
      Nothing -> ""
      Just typing -> concat ["  def ", f, "(", params, ") = ", printExpr e, ";"]
                       where params = intercalate ", " (map (\(x,t) -> x ++ " : " ++ t) (zip xs typing))

                             printExpr (LitInt i) = show i
                             printExpr (LitDouble d) = show d
                             printExpr (LitChar c) = show c
                             printExpr (LitString s) = s 
                             printExpr (App f []) = if (f `elem` xs) then f else f ++ "()"
                             printExpr (App f xs) = f ++ "(" ++ intercalate ", " (map printExpr xs) ++ ")"

checkArg :: CF -> [String] -> Exp -> Cat -> Maybe [(String, Cat)]
checkArg cf vars (LitInt _) (TokenCat "Integer") = Just []
checkArg cf vars (LitDouble _) (TokenCat "Double") = Just []
checkArg cf vars (LitChar _) (TokenCat "Char") = Just []
checkArg cf vars (LitString _) (TokenCat "String") = Just []
checkArg cf vars (App f []) cat =
  if f `elem` vars then Just [(f, cat)] else Just [] -- Actually perform a check here?
checkArg cf vars (App f args) cat = do
  acs <- argCats cf f
  argConstraints <- sequence $ map (\(a, ac) -> checkArg cf vars a ac) (zip args acs)
  return $ concat argConstraints

