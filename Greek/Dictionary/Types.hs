{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Greek.Dictionary.Types (
  MorphEntry(..)
  , Noun(..)
  , Verb(..)
  , Preposition(..)
  , Adverb(..)
  , Participle(..)
  , Adjective(..)
  , Pronoun(..)
  , NumberGr(..)
  , Gender(..)
  , Case(..)
  , Dialect
  , Person(..)
  , Tense(..)
  , Mood(..)
  , Voice(..)
  , MorphLookup
  , Morphology(..)
  , Infinitive(..)
  , Exclamation(..)
  , Adverbial(..)
  , Conjunction(..)
  , Particle(..)
  , Article(..)
  , Numeral(..)
  , Degree(..)
  , Irregular(..)
  , Feature
  , Key
  , Value
  , Database(..)
  ) where

import Data.Typeable
import Data.SafeCopy
import qualified Data.Text as T
import qualified Data.Map as M

data NumberGr = Singular 
        | Plural 
        | Dual deriving (Eq,Show,Typeable)

data Degree = NoDegree
        | Comparative
        | Superlative deriving (Eq,Show,Typeable)

data Gender = Masculine 
      | Feminine 
      | Neuter deriving (Eq,Show,Typeable)

data Case = Nominative 
      | Genitive 
      | Accusative 
      | Dative 
      | Vocative deriving (Eq,Show,Typeable)

type Dialect = T.Text

data Person = First 
      | Second 
      | Third deriving (Eq,Show,Typeable)

data Tense = Present 
       | Imperfect 
       | Aorist 
       | Perfect
       | Future
       | FuturePerfect
       | Pluperfect deriving (Eq,Show,Typeable)

data Mood =  Indicative
       | Imperative
       | Subjunctive
       | Optative deriving (Eq,Show,Typeable)

data Voice = Active
       | Passive
       | Middle
       | MidPass deriving (Eq,Show,Typeable)

type Feature = T.Text

type MorphLookup = M.Map T.Text [MorphEntry]

data MorphEntry = MorphEntry {
     lemma      :: T.Text
  ,  wordForm   :: T.Text
  ,  morphology :: Morphology
  ,  dialect    :: [Dialect]
  ,  feature    :: Feature  } deriving (Eq,Show,Typeable)

data Morphology = MorphNoun Noun 
        | MorphVerb Verb 
        | MorphPrep Preposition
        | MorphPart Participle
        | MorphAdj  Adjective
        | MorphAdv  Adverb
        | MorphInf  Infinitive
        | MorphExcl Exclamation
        | MorphPron Pronoun
        | MorphConj Conjunction
        | MorphParc Particle
        | MorphArt  Article
        | MorphNum  Numeral
        | MorphAdvl Adverbial
        | MorphIrr  Irregular deriving (Eq,Show,Typeable)

data Noun = Noun {
    nounNumber     :: Maybe NumberGr
  , nounGender     :: Maybe Gender
  , nounCase       :: Maybe Case
} deriving (Eq,Show,Typeable)

data Verb = Verb {
    verbPerson  :: Maybe Person
  , verbNumber  :: Maybe NumberGr
  , verbTense   :: Maybe Tense
  , verbMood    :: Maybe Mood
  , verbVoice   :: Maybe Voice
} deriving (Eq,Show,Typeable)

data Preposition = Preposition {
   prepDegree :: Maybe Degree
} deriving (Eq,Show,Typeable)

data Exclamation = Exclamation deriving (Eq,Show,Typeable)

data Participle = Participle {
    partNumber  :: Maybe NumberGr
  , partTense   :: Maybe Tense
  , partVoice   :: Maybe Voice
  , partGender  :: Maybe Gender
  , partCase    :: Maybe Case
} deriving (Eq,Show,Typeable)

data Adjective = Adjective {
    adjNumber  ::  Maybe NumberGr
  , adjGender  ::  Maybe Gender
  , adjCase    ::  Maybe Case
  , adjDeg     ::  Maybe Degree
} deriving (Eq,Show,Typeable)

data Adverb = Adverb {
    advDegree :: Maybe Degree
} deriving (Eq,Show,Typeable)

data Pronoun = Pronoun {
    pronPerson  :: Maybe Person
  , pronNumber  :: Maybe NumberGr
  , pronGender  :: Maybe Gender
  , pronCase    :: Maybe Case
} deriving (Eq,Show,Typeable)

data Infinitive = Infinitive {
    infTense :: Maybe Tense
  , infVoice :: Maybe Voice
} deriving (Eq,Show,Typeable)

data Article = Article {
    artNumber :: Maybe NumberGr
  , artGender :: Maybe Gender
  , artCase   :: Maybe Case
} deriving (Eq,Show,Typeable)

data Adverbial = Adverbial {
  advlDegree :: Maybe Degree
} deriving (Eq,Show,Typeable)

data Conjunction = Conjunction deriving (Eq,Show,Typeable)

data Particle = Particle deriving (Eq,Show,Typeable)

data Numeral = Numeral {
    numlNumber :: Maybe NumberGr
  , numlGender :: Maybe Gender
  , numlCase   :: Maybe Case 
} deriving (Eq,Show,Typeable)

data Irregular = Irregular deriving (Eq,Show,Typeable)

type Key = T.Text
type Value = MorphEntry

data Database = Database !(M.Map Key [Value])
  deriving (Show,Eq,Typeable)

$(deriveSafeCopy 0 'base ''Case)
$(deriveSafeCopy 0 'base ''Degree)
$(deriveSafeCopy 0 'base ''NumberGr)
$(deriveSafeCopy 0 'base ''Gender)
$(deriveSafeCopy 0 'base ''Tense)
$(deriveSafeCopy 0 'base ''Voice)
$(deriveSafeCopy 0 'base ''Mood)
$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''Adverb)
$(deriveSafeCopy 0 'base ''Adverbial)
$(deriveSafeCopy 0 'base ''Conjunction)
$(deriveSafeCopy 0 'base ''Infinitive)
$(deriveSafeCopy 0 'base ''Irregular)
$(deriveSafeCopy 0 'base ''Numeral)
$(deriveSafeCopy 0 'base ''Pronoun)
$(deriveSafeCopy 0 'base ''Adjective)
$(deriveSafeCopy 0 'base ''Noun)
$(deriveSafeCopy 0 'base ''Exclamation)
$(deriveSafeCopy 0 'base ''Preposition)
$(deriveSafeCopy 0 'base ''Article)
$(deriveSafeCopy 0 'base ''Participle)
$(deriveSafeCopy 0 'base ''Particle)
$(deriveSafeCopy 0 'base ''Verb)
$(deriveSafeCopy 0 'base ''Morphology)
$(deriveSafeCopy 0 'base ''MorphEntry)
$(deriveSafeCopy 0 'base ''Database)