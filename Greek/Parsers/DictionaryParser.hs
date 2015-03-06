{-# LANGUAGE OverloadedStrings #-}

module Greek.Parsers.DictionaryParser (
	nounParser
  , verbParser
  , adjParser
  , partParser
  , pronounParser
  , infParser
  , advParser
  , entryParser
  , parseHeader
	) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Greek.Dictionary.Types

parseHeader :: Parser ()
parseHeader = do
	manyTill anyChar $ string "analyses>"
	skipSpace

dialectParser :: Parser [Dialect]
dialectParser = xmlWrapper "dialect" $
	T.words <$> takeTill (=='<')

featureParser :: Parser Feature
featureParser = xmlWrapper "feature" $ takeTill (=='<')

numParser :: Parser NumberGr
numParser = xmlWrapper "number" $ parserMap [
		( "sg" , Singular)
	  , ("dual", Dual    )
	  , ("pl"  , Plural  )
	]

genderParser :: Parser Gender
genderParser = xmlWrapper "gender" $ parserMap [
		("masc", Masculine)
	  , ("fem" , Feminine )
	  , ("neut",  Neuter  )
	]

caseParser :: Parser Case
caseParser = xmlWrapper "case" $ parserMap [
		("nom" , Nominative)
	  ,	("acc" , Accusative)
	  ,	("gen" , Genitive  )
	  ,	("voc" , Vocative  )
	  ,	("dat" , Dative    )
	]

personParser :: Parser Person
personParser = xmlWrapper "person" $ parserMap [
	   ("1st" , First)
	 , ("2nd" , Second)
	 , ("3rd" , Third)
	 ]

tenseParser :: Parser Tense
tenseParser = xmlWrapper "tense" $ parserMap [
	   ("pres" , Present)
	 , ("imperf" , Imperfect)
	 , ("fut" , Future)
	 , ("aor" , Aorist)
	 , ("perf" , Perfect)
	 , ("plup" , Pluperfect)
	 ]

moodParser :: Parser Mood
moodParser = xmlWrapper "mood" $ parserMap [
	   ("ind"     , Indicative )
	 , ("imperat" , Imperative )
	 , ("subj"    , Subjunctive)
	 , ("opt"     , Optative   )
	 ]

voiceParser :: Parser Voice
voiceParser = xmlWrapper "voice" $ parserMap [
	   ("act" , Active )
	 , ("pass", Passive)
	 , ("mid" , Middle )
	 ]

entryParser :: Parser MorphEntry
entryParser = tok $ xmlWrapper "analysis" $ tok $ do
	frm <- xmlWrapper "form"  $ takeTill (=='<')
	lma <- xmlWrapper "lemma" $ takeTill (=='<')
	mrph <- tok $ (MorphNoun <$> nounParser)
		<|> (MorphVerb <$> verbParser)
		<|> (MorphPron <$> pronounParser)
		<|> (MorphAdj  <$> adjParser)
		<|> (MorphPart <$> partParser)
		<|> (MorphInf  <$> infParser)
		<|> (MorphAdv  <$> advParser)
	return $ MorphEntry frm lma mrph

verbParser :: Parser Verb
verbParser = string "<pos>verb</pos>" >>
	Verb <$> personParser
		 <*> numParser
		 <*> tenseParser
		 <*> moodParser
		 <*> voiceParser
		 <*> option [T.empty] dialectParser
		 <*> option T.empty featureParser

infParser :: Parser Infinitive
infParser = string "<pos>verb</pos>" >>
	Infinitive <$> tenseParser
			   <*> (string "<mood>inf</mood>" >> voiceParser)
		       <*> option [T.empty] dialectParser
		       <*> option T.empty featureParser


nounParser :: Parser Noun
nounParser = string "<pos>noun</pos>" >>
	Noun <$> numParser 
		 <*> genderParser
		 <*> caseParser 
		 <*> option [T.empty] dialectParser 
		 <*> option T.empty featureParser

adjParser :: Parser Adjective
adjParser = string "<pos>adj</pos>" >>
	Adjective <$> numParser 
		      <*> genderParser
		      <*> caseParser 
		      <*> option [T.empty] dialectParser 
		      <*> option T.empty featureParser

partParser :: Parser Participle
partParser = string "<pos>part</pos>" >>
	Participle <$> numParser 
		       <*> tenseParser
		       <*> voiceParser
		       <*> genderParser
		       <*> caseParser 
		       <*> option [T.empty] dialectParser 
		       <*> option T.empty featureParser

advParser :: Parser Adverb
advParser = string "<pos>adv</pos>" >>
	Adverb <$> option T.empty featureParser

pronounParser :: Parser Pronoun
pronounParser = string "<pos>pron</pos>" >>
	Pronoun <$> numParser 
		    <*> genderParser
		    <*> caseParser 
		    <*> option [T.empty] dialectParser 
		    <*> option T.empty featureParser

xmlWrapper :: T.Text -> Parser a -> Parser a
xmlWrapper st p = do
	char '<'
	string st
	char '>'
	val <- p
	string "</"
	string st
	char '>'
	return val

parserMap :: (Monad f, Alternative f) => [(f a1, a)] -> f a
parserMap xs = choice $ map (\(a,b) -> a *> return b) xs

tok :: Parser a -> Parser a
tok p = do
	skipSpace
	x <- p
	skipSpace
	return x