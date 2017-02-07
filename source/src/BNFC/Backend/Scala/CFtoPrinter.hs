{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2004  Author:  Aarne Ranta
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

module BNFC.Backend.Scala.CFtoPrinter (cf2Printer) where

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Scala.ScalaCommon
import Data.Char(toLower)
import Data.Either(lefts)
import Data.Maybe(fromJust)
import Text.PrettyPrint

cf2Printer :: String -> String -> CF -> String
cf2Printer pkgBase lname cf = unlines
  [ "/* Generated by BNFC */"
  , ""
  , "package " ++ pkgBase
  , ""
  , "class " ++ lname ++ "PPrint {"
  , ""
  , "  import " ++ lname ++ "Syntax._"
  , ""
  , "  sealed trait Token"
  , "  case class Phrase(seq: Token*) extends Token"
  , "  case class Literal(str: String) extends Token"
  , "  case class Delim(ld: String, t: Token, rd: String) extends Token"
  , ""
  , "  def printToken(t : Token) : String = "
  , "    t match {"
  , "      case Literal(str) => str"
  , "      case Phrase(seq @ _*) => seq.map(printToken(_)).mkString(\" \")"
  , "      case Delim(ld, t, rd) => ld + printToken(t) + rd"
  , "    }"
  , ""
  , "  def pprint[A](a: A)(implicit tt: Tokenizable[A]): String ="
  , "    printToken(tt.tokenize(a))"
  , ""
  , "  trait Tokenizable[A] {"
  , "    def tokenize(a: A): Token"
  , "  }"
  , ""
  , "  implicit class TokenizableOps[A](a: A)(implicit tt: Tokenizable[A]) {"
  , ""
  , "    def parenthesize: Token = "
  , "      tt.tokenize(a) match {"
  , "        case p@Phrase(seq @ _*) => if (seq.length > 1) Delim(\"(\", p, \")\") else p"
  , "        case t => t"
  , "      }"
  , ""
  , "    def tokenize: Token ="
  , "      tt.tokenize(a)"
  , "" 
  , "  }"
  , ""
  , "  implicit val isTokenizableString : Tokenizable[String] = "
  , "    new Tokenizable[String] {"
  , "      def tokenize(s: String) = Literal(s)"
  , "    }"
  , ""
  , rules cf
  , ""
  , "}"
  ]

rules :: CF -> String
rules cf = unlines $
  map (\(s,xs) -> render (case_fun s (map toArgs xs)) ++++ ifList cf s) $ cf2data cf
  where
    toArgs (cons,_) = (cons, ruleOf cons)
    ruleOf s = fromJust $ lookupRule s (cfgRules cf)


case_fun :: Cat -> [(String, (Cat, [Either Cat String]))] -> Doc
case_fun cat xs = vcat
  [ "  def tokenize" <> type_ <> "(a: " <> type_ <> "): Token ="
  , nest 4 $ vcat
    [ "a match {"
    , nest 2 $ vcat (map mkPrintCase xs)
    , "}"
    ]
  , ""
  , "  implicit val isTokenizable" <> type_ <+> " : Tokenizable[" <> type_ <> "] ="
  , "    new Tokenizable[" <> type_ <> "] {"
  , "      def tokenize(a:" <+> type_ <> "): Token = tokenize" <> type_ <> "(a)"
  , "    }"
  ]

  where type_ = text (varName cat)

mkPrintCase :: (Fun, (Cat, [Either Cat String])) -> Doc
mkPrintCase (f, (cat, rhs)) =
  "case" <+> text f <> parens (hsep (punctuate (text ",") variables))
  <+> "=>" <+> mkRhs (map render variables) rhs 
  where
    -- Creating variables names used in pattern matching. In addition to
    -- haskell's reserved words, `a` and `i` are used in the printing function
    -- and should be avoided
    names = map var (filter (/=InternalCat) $ lefts rhs)
    variables = map text $ mkNames ("a":"i":[]) LowerCase names
    var (ListCat c)  = var c ++ "s"
    var (TokenCat "Ident")   = "id"
    var (TokenCat "Integer") = "n"
    var (TokenCat "String")  = "str"
    var (TokenCat "Char")    = "c"
    var (TokenCat "Double")  = "d"
    var xs        = map toLower $ show xs

ifList :: CF -> Cat -> String
ifList cf cat = render $ nest 2 $ vcat listDef
  where
    type_ = text (varName cat)
    rules = rulesForNormalizedCat cf (ListCat cat)
    cases = [ mkPrtListCase r | r <- rules ]
    listDef =  if (length cases > 0)
               then [ ""
                    , "def tokenizeList" <> type_ <> parens ("a: List[" <> type_ <> "]") <> ": Token ="
                    , "  a match {"
                    , "    case Nil => Phrase()"
                    , nest 4 $ vcat cases
                    , "  }"
                    , ""
                    , "implicit val isTokenizableList" <> type_ <+> " : Tokenizable[List[" <> type_ <> "]] ="
                    , "  new Tokenizable[List[" <> type_ <> "]] {"
                    , "    def tokenize(l: List[" <> type_ <> "]): Token = tokenizeList" <> type_ <> "(l)"
                    , "  }"
                    , ""
                    ]
               else []

mkPrtListCase :: Rule -> Doc
mkPrtListCase (Rule f (ListCat c) rhs)
  | isNilFun f = empty -- "case" <+> "Nil" <+> "=>" <+> body
  | isOneFun f = "case" <+> "x :: Nil" <+> "=>" <+> body
  | isConsFun f = "case" <+> "x :: xs" <+> "=>" <+> body
  | otherwise = empty -- (++) constructor
  where
    body = mkRhs ["x", "xs"] rhs
mkPrtListCase _ = error "mkPrtListCase undefined for non-list categories"


mkRhs :: [String] -> [Either Cat String] -> Doc
mkRhs args its = "Phrase" <> parens (hsep (punctuate "," (mk args its))) 
  where
    mk args (Left InternalCat : items) = mk args items
    mk (arg:args) (Left c  : items)    = (prt arg c) : mk args items
    mk args       (Right s : items)    = "Literal(" <> text (show s) <> ")" : mk args items
    mk _          _                    = []
    prt a c = text a <> ".parenthesize"
