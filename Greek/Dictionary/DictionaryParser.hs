{-# LANGUAGE OverloadedStrings #-}

module Greek.Dictionary.DictionaryParser (
	) where

import Data.Char
import qualified Data.Text as T
import Data.Attoparsec.Text
import Greek.Dictionary.Types
import Control.Applicative

numParser :: Parser NumberGr
numParser = do
	word <- xmlWrapperAlpha "number"
	return $ case word of
		"sg" -> Singular
		"dual" -> Dual
		"pl" -> Plural
		_ -> error $ "unexpected number"

genderParser :: Parser Gender
genderParser = xmlWrapperAlpha "gender" >>=
		choice [
			"masc" *> return Masculine
		  , "fem"  *> return Feminine
		  , "neut" *> return Neuter]


--nounParser :: Parser Noun
--nounParser = do
--	string "<pos>noun</pos>"
--	num <- numParser
--	g <- genderParser


xmlWrapperAlpha :: T.Text -> Parser T.Text	
xmlWrapperAlpha st = do
	char '<'
	string st
	char '>'
	val <- takeWhile1 isAlpha
	string "</"
	string st
	char '>'
	return val

