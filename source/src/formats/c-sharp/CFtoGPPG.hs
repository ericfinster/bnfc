{-
    BNF Converter: C# GPPG Generator
    Copyright (C) 2006  Author:  Johan Broberg
    
    Modified from CFtoBisonSTL.

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
    BNF Converter Module

    Description   : This module generates the GPPG input file.

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 24 November, 2006

    Modified      : 17 December, 2006 by Johan Broberg

   ************************************************************** 
-}

module CFtoGPPG (cf2gppg) where

import CF
import Data.List (intersperse, isPrefixOf)
import NamedVariables hiding (varName)
import Data.Char (toLower,isUpper,isDigit)
import Utils ((+++), (++++))
import TypeChecker
import ErrM
import OOAbstract hiding (basetypes)
import CSharpUtils

--This follows the basic structure of CFtoHappy.

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern,Action)])]
type NonTerminal = String
type Pattern     = String
type Action      = String
type MetaVar     = String

--The environment comes from the CFtoGPLEX
cf2gppg :: Namespace -> CF -> SymEnv -> String
cf2gppg namespace cf env = unlines [
  header namespace cf,
  union namespace (positionCats cf ++ allCats cf ++ tokentypes (cf2cabs cf)),
  tokens user env,
  declarations cf,
  "",
  specialToks cf,
  "",
  "%%",
  prRules (rulesForGPPG namespace cf env)
  ]
  where
    user = fst (unzip (tokenPragmas cf))

positionCats cf = filter (isPositionCat cf) $ fst (unzip (tokenPragmas cf))

header :: Namespace -> CF -> String
header namespace cf = unlines [
  "/* This GPPG file was machine-generated by BNFC */",
  "",
  "%namespace " ++ namespace,
  "%{",
  definedRules namespace cf,
  unlinesInline $ map (parseMethod namespace) (allCatsIdNorm cf ++ positionCats cf),
  "%}"
  ]

definedRules :: Namespace -> CF -> String
definedRules namespace cf@((pr,_),_) = unlinesInline [
  if null [ rule f xs e | FunDef f xs e <- pr ] 
    then ""
    else error "Defined rules are not yet available in C# mode!"
  ]
  where
    ctx = buildContext cf

    list = LC (const "[]") (\t -> "List" ++ unBase t)
      where
        unBase (ListT t) = unBase t
        unBase (BaseT x) = normCat x

    rule f xs e =
      case checkDefinition' list ctx f xs e of
        Bad err	-> error $ "Panic! This should have been caught already:\n" ++ err
        Ok (args,(e',t)) -> unlinesInline [
          "Defined Rule goes here"
          ]

--This generates a parser method for each entry point.
parseMethod :: Namespace -> Cat -> String
parseMethod namespace cat = unlinesInline [
  "  " ++ returntype +++ returnvar ++ " = null;",
  "  public " ++ returntype ++ " Parse" ++ cat' ++ "()",
  "  {",
  "    if(this.Parse())",
  "    {",
  "      return " ++ returnvar ++ ";",
  "    }",
  "    else",
  "    {",
  "      throw new Exception(\"Could not parse input stream!\");",
  "    }",
  "  }",
  "  "
  ]
  where
    cat' = identCat (normCat cat)
    returntype = identifier namespace cat'
    returnvar = resultName cat'

--The union declaration is special to GPPG/GPLEX and gives the type of yylval.
--For efficiency, we may want to only include used categories here.
union :: Namespace -> [Cat] -> String
union namespace cats = unlines $ filter (\x -> x /= "\n") [
  "%union",
  "{",
  "  public int int_;",
  "  public char char_;",
  "  public double double_;",
  "  public string string_;",
  unlinesInline $ map catline cats,
  "}"
  ]
  where --This is a little weird because people can make [Exp2] etc.
    catline cat | (identCat cat /= cat) || (normCat cat == cat) = 
      "  public " ++ identifier namespace (identCat (normCat cat)) +++ (varName (normCat cat)) ++ ";"
    catline cat = ""

--declares non-terminal types.
declarations :: CF -> String
declarations cf = unlinesInline $ map (typeNT cf) (positionCats cf ++ allCats cf)
 where --don't define internal rules
   typeNT cf nt | (isPositionCat cf nt || rulesForCat cf nt /= []) = "%type <" ++ (varName (normCat nt)) ++ "> " ++ (identCat nt)
   typeNT cf nt = ""

--declares terminal types.
tokens :: [UserDef] -> SymEnv -> String
tokens user ts = concatMap (declTok user) ts
  where
    declTok u (s,r) = if elem s u
      then "%token<" ++ varName (normCat s) ++ "> " ++ r ++ "   //   " ++ s ++ "\n"
      else "%token " ++ r ++ "    //   " ++ s ++ "\n"

specialToks :: CF -> String
specialToks cf = unlinesInline [ 
  ifC "String"  "%token<string_> STRING_",
  ifC "Char"    "%token<char_> CHAR_",
  ifC "Integer" "%token<int_> INTEGER_",
  ifC "Double"  "%token<double_> DOUBLE_",
  ifC "Ident"   "%token<string_> IDENT_"
  ]
  where
    ifC cat s = if isUsedCat cf cat then s else ""

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForGPPG :: Namespace -> CF -> SymEnv -> Rules
rulesForGPPG namespace cf env = (map mkOne $ ruleGroups cf) ++ posRules where
  mkOne (cat,rules) = constructRule namespace cf env rules cat
  posRules = map mkPos $ positionCats cf
  mkPos cat = (cat, [(maybe cat id (lookup cat env),
    "$$ = new " ++ cat ++ "($1);")])

-- For every non-terminal, we construct a set of rules.
constructRule :: Namespace -> 
  CF -> SymEnv -> [Rule] -> NonTerminal -> (NonTerminal,[(Pattern,Action)])
constructRule namespace cf env rules nt = 
  (nt,[(p,(generateAction namespace nt (ruleName r) b m) +++ result) | 
     r0 <- rules,
     let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs 
                   then (True,revSepListRule r0) 
                 else (False,r0),
     let (p,m) = generatePatterns cf env r b])
  where
    ruleName r = case funRule r of
      ---- "(:)" -> identCat nt
      ---- "(:[])" -> identCat nt
      z -> z
    revs = reversibleCats cf
    eps = allEntryPoints cf
    isEntry nt = if elem nt eps then True else False
    result = if isEntry nt then (resultName (normCat (identCat nt))) ++ "= $$;" else ""

-- Generates a string containing the semantic action.
-- This was copied from CFtoCup15, with only a few small modifications
generateAction :: Namespace -> NonTerminal -> Fun -> Bool -> [(MetaVar, Bool)] 
	       -> Action
generateAction namespace nt f rev mbs
  | isNilFun f             = "$$ = new " ++ identifier namespace c ++ "();"
  | isOneFun f             = "$$ = new " ++ identifier namespace c ++ "(); $$.Add(" ++ p_1 ++ ");"
  | isConsFun f && not rev = "$$ = " ++ p_2 ++ "; " ++ p_2 ++ ".Insert(0, " ++ p_1 ++ ");"
  | isConsFun f && rev     = "$$ = " ++ p_1 ++ "; " ++ p_1 ++ ".Add(" ++ p_2 ++ ");"
  | isCoercion f           = "$$ = " ++ p_1 ++ ";"
  | isDefinedRule f        = "$$ = " ++ f ++ "_" ++ "(" ++ concat (intersperse "," ms) ++ ");"
  | otherwise              = "$$ = new " ++ identifier namespace c ++ "(" ++ concat (intersperse "," ms) ++ ");"
  where
    c = if isNilFun f || isOneFun f || isConsFun f 
        then identCat (normCat nt) else f
    ms = map fst mbs
    p_1 = ms!!0
    p_2 = ms!!1


-- Generate patterns and a set of metavariables indicating 
-- where in the pattern the non-terminal
generatePatterns :: CF -> SymEnv -> Rule -> Bool -> (Pattern,[(MetaVar,Bool)])
generatePatterns cf env r revv = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its) 
  where
    mkIt i = case i of
      Left c -> case lookup c env of
        -- This used to be x, but that didn't work if we had a symbol "String" in env, and tried to use a normal String - it would use the symbol...
        Just x | not (isPositionCat cf c) && c `notElem` (map fst basetypes) -> x
        _ -> typeName (identCat c)
      Right s -> case lookup s env of
        Just x -> x
        Nothing -> s
    metas its = [('$': show i,revert c) | (i,Left c) <- zip [1 :: Int ..] its]

    -- notice: reversibility with push_back vectors is the opposite
    -- of right-recursive lists!
    revert c = (head c == '[') && 
               not (isConsFun (funRule r)) && notElem c revs 
    revs = reversibleCats cf

-- We have now constructed the patterns and actions, 
-- so the only thing left is to merge them into one string.

prRules :: Rules -> String
prRules [] = []
prRules ((nt, []):rs) = prRules rs --internal rule
prRules ((nt,((p,a):ls)):rs) = 
  (unwords [nt', ":" , p, "{ ", a, "}", "\n" ++ pr ls]) ++ ";\n" ++ prRules rs
  where 
    nt' = identCat nt
    pr []           = []
    pr ((p,a):ls)   = (unlines [(concat $ intersperse " " ["  |", p, "{ ", a , "}"])]) ++ pr ls

--Some helper functions.
resultName :: String -> String
resultName s = "YY_RESULT_" ++ s ++ "_"

--slightly stronger than the NamedVariable version.
varName :: String -> String
varName s = (map toLower (identCat s)) ++ "_"

typeName :: String -> String
typeName "Ident" = "IDENT_"
typeName "String" = "STRING_"
typeName "Char" = "CHAR_"
typeName "Integer" = "INTEGER_"
typeName "Double" = "DOUBLE_"
typeName x = x
