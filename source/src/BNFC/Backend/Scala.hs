{-
    BNF Converter: Scala Top File
    Copyright (C) 2004  Author:  Markus Forsberg, Peter Gammie,
                                 Michael Pellauer, Bjorn Bringert
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

-------------------------------------------------------------------
-- |
-- Module      :  ScalaTop
-- Copyright   :  (C)opyright 2003, {markus, aarne, pellauer, peteg, bringert} at cs dot chalmers dot se
-- Copyright   :  (C)opyright 2016, ericfinster at gmail dot com
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  ericfinster at gmail dot com
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for the Scala back end.
--
-------------------------------------------------------------------

module BNFC.Backend.Scala ( makeScala ) where

import BNFC.CF
import BNFC.Utils
import BNFC.Options 
import BNFC.Backend.Base
import BNFC.Backend.Scala.CFtoAbstract
import BNFC.Backend.Scala.CFtoJLex15
import BNFC.Backend.Scala.CFtoBisonScala

-- import Data.List (intercalate)
-- import Data.Maybe (catMaybes)
import System.FilePath (pathSeparator)

-- naming conventions

-- absFile, lexFile, bisonFile :: Options -> String
-- absFile = mkFile withLang "Syntax" "scala"
-- lexFile = mkFile withLang "Lexer" "flex"
-- bisonFile = mkFile withLang "Parser" "y"

-- mkFile :: (Options -> String -> String) -> String -> String -> Options -> FilePath
-- mkFile addLang name ext opts = pkgToDir (mkMod addLang name opts) <.> ext

-- mkMod :: (Options -> String -> String) -> String -> Options -> String
-- mkMod addLang name opts = mkNamespace opts <.> mod
--   where
--     [] <.> s = s
--     s1 <.> s2 = s1 ++ "." ++ s2
--     mod | inDir opts = name
--         | otherwise  = addLang opts name

-- noLang :: Options -> String -> String
-- noLang _ name = name

-- mkNamespace :: Options -> FilePath
-- mkNamespace opts = intercalate "." $ catMaybes [inPackage opts, dir]
--   where
--     dir | inDir opts = Just (mkName [] CamelCase (lang opts))
--         | otherwise  = Nothing

-- withLang :: Options -> String -> String
-- withLang opts name = name ++ (mkName [] CamelCase (lang opts)) 

pkgToDir :: String -> FilePath
pkgToDir s = replace '.' pathSeparator s

makeScala :: SharedOptions -> CF -> MkFiles ()
makeScala opts cf = do
  mkfile (pkgDir ++ "/" ++ cname ++ "Syntax.scala") (cf2Abstract pkgName cf)
  let (lexDoc, env) = cf2jlex pkgName cname cf
  mkfile (pkgDir ++ "/" ++ cname ++ ".flex") lexDoc
  mkfile (pkgDir ++ "/" ++ cname ++ ".y") (cf2Bison pkgName cf env)

  where lname = mkName [] LowerCase (lang opts)
        cname = mkName [] CamelCase (lang opts)

        pkgName = case inPackage opts of
                    Just p -> p ++ "." ++ lname
                    Nothing -> lname

        pkgDir = pkgToDir pkgName
