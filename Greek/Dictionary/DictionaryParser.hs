{-# LANGUAGE OverloadedStrings #-}

module Greek.Dictionary.DictionaryParser (
	) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Greek.Dictionary.Types

dialectParser :: Parser [Dialect]
dialectParser = do
	stringVal <- xmlWrapper "dialect"
	return $ T.words stringVal

featureParser :: Parser Feature
featureParser = xmlWrapper "feature"

numParser :: Parser NumberGr
numParser = do
	word <- xmlWrapper "number"
	return $ case word of
		"sg" -> Singular
		"dual" -> Dual
		"pl" -> Plural
		_ -> error $ "unexpected number"

genderParser :: Parser Gender
genderParser = do
	word <- xmlWrapper "gender"
	return $ case word of
		"masc" -> Masculine
		"fem" ->  Feminine
		"neut" -> Neuter
		_ -> error "invalid gender."

caseParser :: Parser Case
caseParser = do
	word <- xmlWrapper "case"
	return $ case word of
		"nom" -> Nominative
		"acc" -> Accusative
		"gen" -> Genitive
		"voc" -> Vocative
		"dat" -> Dative
		_ -> error "invalid case."


nounParser :: Parser Noun
nounParser = do
	string "<pos>noun</pos>"
	num <- numParser
	g <- genderParser
	c <- caseParser
	d <- option [T.empty] dialectParser
	f <- option T.empty featureParser
	return $ Noun num g c d f





xmlWrapper :: T.Text -> Parser T.Text	
xmlWrapper st = do
	char '<'
	string st
	char '>'
	val <- takeTill (=='<')
	string "</"
	string st
	char '>'
	return val

