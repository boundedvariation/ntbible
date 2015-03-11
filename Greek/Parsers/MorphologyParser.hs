{-# LANGUAGE OverloadedStrings #-}

module Greek.Parsers.MorphologyParser (
    nounParser
  , verbParser
  , adjParser
  , partParser
  , pronounParser
  , infParser
  , advParser
  , entryParser
  , parseHeader
  , morphParser
    ) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Greek.Dictionary.Types

morphParser :: Parser [MorphEntry]
morphParser = do
    parseHeader
    many' entryParser

parseHeader :: Parser ()
parseHeader = do
    manyTill anyChar $ string "analyses>"
    skipSpace

dialectParser :: Parser [Dialect]
dialectParser = xmlWrapper "dialect" $
    T.words <$> takeTill (=='<')

featureParser :: Parser Feature
featureParser = xmlWrapper "feature" $ takeTill (=='<')

numParser :: Parser (Maybe NumberGr)
numParser = option Nothing 
    $ xmlWrapper "number" 
    $ Just <$> parserMap [
        ("sg"  , Singular)
      , ("dual", Dual    )
      , ("pl"  , Plural  )
    ]

genderParser :: Parser (Maybe Gender)
genderParser = option Nothing
    $ xmlWrapper "gender" 
    $ Just <$> parserMap [
        ("masc", Masculine)
      , ("fem" , Feminine )
      , ("neut",  Neuter  )
    ]

caseParser :: Parser (Maybe Case)
caseParser = option Nothing
  $ xmlWrapper "case" 
  $ Just <$> parserMap [
        ("nom" , Nominative)
      , ("acc" , Accusative)
      , ("gen" , Genitive  )
      , ("voc" , Vocative  )
      , ("dat" , Dative    )
    ]

personParser :: Parser (Maybe Person)
personParser = option Nothing
  $ xmlWrapper "person" 
  $ Just <$> parserMap [
       ("1st" , First)
     , ("2nd" , Second)
     , ("3rd" , Third)
     ]

tenseParser :: Parser (Maybe Tense)
tenseParser = option Nothing 
    $ xmlWrapper "tense" 
    $ Just <$> parserMap [
       ("pres"   , Present)
     , ("imperf" , Imperfect)
     , ("futperf", FuturePerfect)
     , ("fut"    , Future)
     , ("aor"    , Aorist)
     , ("perf"   , Perfect)
     , ("plup"   , Pluperfect)
     ]

moodParser :: Parser (Maybe Mood)
moodParser = option Nothing
  $ xmlWrapper "mood" 
  $ Just <$> parserMap [
       ("ind"     , Indicative )
     , ("imperat" , Imperative )
     , ("subj"    , Subjunctive)
     , ("opt"     , Optative   )
     ]

voiceParser :: Parser (Maybe Voice)
voiceParser = option Nothing
  $ xmlWrapper "voice" 
  $ Just <$> parserMap [
       ("act" , Active )
     , ("pass", Passive)
     , ("mid" , Middle )
     , ("mp"  , MidPass)
     ]

degParser :: Parser (Maybe Degree)
degParser = option Nothing
    $ xmlWrapper "degree" 
    $ Just <$> parserMap [
       ("comp" , Comparative )
     , ("superl"  , Superlative)
     ]

entryParser :: Parser MorphEntry
entryParser = tok $ xmlWrapper "analysis" $ tok $ do
    frm <- xmlWrapper "form"  $ takeTill (=='<')
    lma <- xmlWrapper "lemma" $ takeTill (=='<')
    mrph <- tok $ (MorphNoun <$> nounParser)
        <|> (MorphInf  <$> infParser)
        <|> (MorphVerb <$> verbParser)
        <|> (MorphPron <$> pronounParser)
        <|> (MorphAdj  <$> adjParser)
        <|> (MorphPart <$> partParser)
        <|> (MorphAdv  <$> advParser)
        <|> (MorphPrep <$> prepParser)
        <|> (MorphExcl <$> exclParser)
        <|> (MorphAdvl <$> advlParser)
        <|> (MorphConj <$> conjParser)
        <|> (MorphParc <$> parcParser)
        <|> (MorphArt  <$> artParser)
        <|> (MorphNum  <$> numeralParser)
        <|> (MorphIrr  <$> irrParser)
    d <- option [T.empty] dialectParser
    f <- option T.empty featureParser 
    return $ MorphEntry frm lma mrph d f

verbParser :: Parser Verb
verbParser = string "<pos>verb</pos>" >>
    Verb <$> personParser
         <*> numParser
         <*> tenseParser
         <*> moodParser
         <*> (voiceParser <* (genderParser >> caseParser)) --for *)/aracon

infParser :: Parser Infinitive
infParser = string "<pos>verb</pos>" >>
    Infinitive <$> tenseParser
               <*> (string "<mood>inf</mood>" >> voiceParser)


nounParser :: Parser Noun
nounParser = string "<pos>noun</pos>" >>
    Noun <$> numParser 
         <*> genderParser
         <*> caseParser 

adjParser :: Parser Adjective
adjParser = string "<pos>adj</pos>" >>
    Adjective <$> numParser 
              <*> genderParser
              <*> caseParser 
              <*> degParser

partParser :: Parser Participle
partParser = string "<pos>part</pos>" >>
    Participle <$> numParser 
               <*> tenseParser
               <*> voiceParser
               <*> genderParser
               <*> caseParser 

pronounParser :: Parser Pronoun
pronounParser = string "<pos>pron</pos>" >>
    Pronoun <$> personParser
            <*> numParser 
            <*> genderParser
            <*> caseParser

artParser :: Parser Article
artParser = string "<pos>article</pos>" >>
    Article <$> numParser 
            <*> genderParser
            <*> caseParser


advParser :: Parser Adverb
advParser = string "<pos>adv</pos>" >> 
    Adverb <$> (degParser <* (numParser >> genderParser >> caseParser)) -- for be/nqosde / o(/n

prepParser :: Parser Preposition
prepParser = string "<pos>prep</pos>" >> 
    Preposition <$> degParser

exclParser :: Parser Exclamation
exclParser = "<pos>exclam</pos>" *> return Exclamation

advlParser :: Parser Adverbial
advlParser = string "<pos>adverbial</pos>" >>
    Adverbial <$> degParser

conjParser :: Parser Conjunction
conjParser = "<pos>conj</pos>" *> return Conjunction

parcParser :: Parser Particle
parcParser = "<pos>partic</pos>" *> return Particle

irrParser :: Parser Irregular
irrParser = do
    string "<pos>irreg</pos>"
    numParser
    genderParser
    caseParser
    return Irregular

numeralParser :: Parser Numeral
numeralParser = do
    string "<pos>numeral</pos>"
    Numeral <$> numParser
            <*> genderParser
            <*> caseParser

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