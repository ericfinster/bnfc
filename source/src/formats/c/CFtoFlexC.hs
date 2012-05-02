{-
    BNF Converter: C flex generator
    Copyright (C) 2004  Author:  Michael Pellauer

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

    Description   : This module generates the Flex file. It is
                    similar to JLex but with a few peculiarities.
                    
    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 5 August, 2003                           

    Modified      : 10 August, 2003                          

   
   ************************************************************** 
-}
module CFtoFlexC (cf2flex) where

import CF
import RegToFlex
-- import Utils((+++), (++++))
import NamedVariables
import Data.List

--The environment must be returned for the parser to use.
cf2flex :: String -> CF -> (String, SymEnv)
cf2flex name cf = (unlines
 [
  prelude name,
  cMacros,
  lexSymbols env,
  restOfFlex cf env'
 ], env')
  where
   env = makeSymEnv (symbols cf ++ reservedWords cf) (0 :: Int)
   env' = env ++ (makeSymEnv (fst (unzip (tokenPragmas cf))) (length env))
   makeSymEnv [] _ = []
   makeSymEnv (s:symbs) n = (s, "_SYMB_" ++ (show n)) : (makeSymEnv symbs (n+1))

prelude :: String -> String
prelude name = unlines
  [
   "/* -*- c -*- This FLex file was machine-generated by the BNF converter */",
   "%option noyywrap",
   "%{",
   "#define yylval " ++ name ++ "lval",
   "#define YY_BUFFER_APPEND " ++ name ++ "_BUFFER_APPEND",
   "#define YY_BUFFER_RESET " ++ name ++ "_BUFFER_RESET",
   "#define initialize_lexer " ++ name ++ "_initialize_lexer",
   "#include <string.h>",
   "#include \"Parser.h\"",
   "#define YY_BUFFER_LENGTH 4096",
   "char YY_PARSED_STRING[YY_BUFFER_LENGTH];",
   "void YY_BUFFER_APPEND(char *s)",
   "{",
   "  strcat(YY_PARSED_STRING, s); //Do something better here!",
   "}",
   "void YY_BUFFER_RESET(void)",
   "{",
   "  int x;",
   "  for(x = 0; x < YY_BUFFER_LENGTH; x++)",
   "    YY_PARSED_STRING[x] = 0;",
   "}",
   "",
   "%}"
  ]

--For now all categories are included.
--Optimally only the ones that are used should be generated.
cMacros :: String
cMacros = unlines
  [
  "LETTER [a-zA-Z]",
  "CAPITAL [A-Z]",
  "SMALL [a-z]",
  "DIGIT [0-9]",
  "IDENT [a-zA-Z0-9'_]",
  "%START YYINITIAL COMMENT CHAR CHARESC CHAREND STRING ESCAPED",
  "%%"
  ]

lexSymbols :: SymEnv -> String
lexSymbols ss = concatMap transSym ss
  where
    transSym (s,r) = 
      "<YYINITIAL>\"" ++ s' ++ "\"      \t return " ++ r ++ ";\n"
        where
         s' = escapeChars s

restOfFlex :: CF -> SymEnv -> String
restOfFlex cf env = concat
  [
   lexComments (comments cf),
   userDefTokens,
   ifC "String" strStates,
   ifC "Char" chStates,
   ifC "Double" "<YYINITIAL>{DIGIT}+\".\"{DIGIT}+(\"e\"(\\-)?{DIGIT}+)?      \t yylval.double_ = atof(yytext); return _DOUBLE_;\n",
   ifC "Integer" "<YYINITIAL>{DIGIT}+      \t yylval.int_ = atoi(yytext); return _INTEGER_;\n",
    ifC "Ident" "<YYINITIAL>{LETTER}{IDENT}*      \t yylval.string_ = strdup(yytext); return _IDENT_;\n"
   , "<YYINITIAL>[ \\t\\r\\n\\f]      \t /* ignore white space. */;\n",
   "<YYINITIAL>.      \t return _ERROR_;\n",
   "%%\n",
   footer
  ]
  where
   ifC cat s = if isUsedCat cf cat then s else ""
   userDefTokens = unlines $
     ["<YYINITIAL>" ++ printRegFlex exp ++ 
      "     \t yylval.string_ = strdup(yytext); return " ++ sName name ++ ";"
       | (name, exp) <- tokenPragmas cf]
      where
          sName n = case lookup n env of
              Just x -> x
              Nothing -> n
   strStates = unlines --These handle escaped characters in Strings.
    [
     "<YYINITIAL>\"\\\"\"      \t BEGIN STRING;",
     "<STRING>\\\\      \t BEGIN ESCAPED;",
     "<STRING>\\\"      \t yylval.string_ = strdup(YY_PARSED_STRING); YY_BUFFER_RESET(); BEGIN YYINITIAL; return _STRING_;",
     "<STRING>.      \t YY_BUFFER_APPEND(yytext);",
     "<ESCAPED>n      \t YY_BUFFER_APPEND(\"\\n\"); BEGIN STRING;",
     "<ESCAPED>\\\"      \t YY_BUFFER_APPEND(\"\\\"\"); BEGIN STRING ;",
     "<ESCAPED>\\\\      \t YY_BUFFER_APPEND(\"\\\\\"); BEGIN STRING;",
     "<ESCAPED>t       \t YY_BUFFER_APPEND(\"\\t\"); BEGIN STRING;",
     "<ESCAPED>.       \t YY_BUFFER_APPEND(yytext); BEGIN STRING;"
    ]
   chStates = unlines --These handle escaped characters in Chars.
    [
     "<YYINITIAL>\"'\" \tBEGIN CHAR;",
     "<CHAR>\\\\      \t BEGIN CHARESC;",
     "<CHAR>[^']      \t BEGIN CHAREND; yylval.char_ = yytext[0]; return _CHAR_;",
     "<CHARESC>n      \t BEGIN CHAREND; yylval.char_ = '\\n'; return _CHAR_;",
     "<CHARESC>t      \t BEGIN CHAREND; yylval.char_ = '\\t'; return _CHAR_;",
     "<CHARESC>.      \t BEGIN CHAREND; yylval.char_ = yytext[0]; return _CHAR_;",
     "<CHAREND>\"'\"      \t BEGIN YYINITIAL;"
    ]
   footer = "void initialize_lexer(FILE *inp) { yyin = inp; BEGIN YYINITIAL; }"

lexComments :: ([(String, String)], [String]) -> String
lexComments (m,s) = 
  (unlines (map lexSingleComment s)) 
  ++ (unlines (map lexMultiComment m))

lexSingleComment :: String -> String
lexSingleComment c = 
  "<YYINITIAL>\"" ++ c ++ "\"[^\\n]*\\n      \t /* BNFC single-line comment */;"

--There might be a possible bug here if a language includes 2 multi-line comments.
--They could possibly start a comment with one character and end it with another.
--However this seems rare.
lexMultiComment :: (String, String) -> String
lexMultiComment (b,e) = unlines [
  "<YYINITIAL>\"" ++ b ++ "\"      \t BEGIN COMMENT;",
  "<COMMENT>\"" ++ e ++ "\"      \t BEGIN YYINITIAL;",
  "<COMMENT>.      \t /* BNFC multi-line comment */;",
  "<COMMENT>[\\n]      \t /* BNFC multi-line comment */;"
  ]
  
lexReserved :: String -> String
lexReserved s = "<YYINITIAL>\"" ++ s ++ "\" \t yylval.string_ = strdup(yytext); return TS;"


--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)
