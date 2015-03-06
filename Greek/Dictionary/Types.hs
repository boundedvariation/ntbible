{-# LANGUAGE OverloadedStrings #-}

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
  , Feature
	) where

import qualified Data.Text as T
import qualified Data.Map as M

data NumberGr = Singular 
			  | Plural 
			  | Dual deriving (Eq, Show)

data Gender = Masculine 
			| Feminine 
			| Neuter
      | NoGender deriving (Eq, Show)

data Case = Nominative 
		  | Genitive 
		  | Accusative 
		  | Dative 
		  | Vocative deriving (Eq, Show)

type Dialect = T.Text

data Person = First 
			| Second 
			| Third
      | NoPerson deriving (Eq, Show)

data Tense = Present 
		   | Imperfect 
		   | Aorist 
		   | Perfect
       | Future
       | FuturePerfect
		   | Pluperfect deriving (Eq, Show)

data Mood =  Indicative
		   | Imperative
		   | Subjunctive
		   | Optative deriving (Eq, Show)

data Voice = Active
		   | Passive
		   | Middle
       | MidPass deriving (Eq, Show)

type Feature = T.Text

type MorphLookup = M.Map T.Text MorphEntry

data MorphEntry = MorphEntry {
	   lemma      :: T.Text
  ,  wordForm   :: T.Text
  ,	 morphology :: Morphology
  ,  dialect    :: [Dialect]
  ,  feature    :: Feature  } deriving (Eq,Show)

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
        | MorphAdvl Adverbial deriving (Eq,Show)

data Noun = Noun {
	  nounNumber     :: NumberGr
  , nounGender     :: Gender
  , nounCase       :: Case
} deriving (Eq,Show)

data Verb = Verb {
	  verbPerson  :: Person
  , verbNumber  :: NumberGr
  , verbTense   :: Tense
  , verbMood    :: Mood
  , verbVoice   :: Voice
} deriving (Eq,Show)

data Preposition = Preposition deriving (Eq,Show)

data Exclamation = Exclamation deriving (Eq,Show)

data Participle = Participle {
	  partNumber  :: NumberGr
  , partTense   :: Tense
  , partVoice   :: Voice
  , partGender  :: Gender
  , partCase    :: Case
} deriving (Eq,Show)

data Adjective = Adjective {
	  adjNumber  ::  NumberGr
  , adjGender  ::  Gender
  , adjCase    ::  Case
} deriving (Eq,Show)

data Adverb = Adverb deriving (Eq,Show)

data Pronoun = Pronoun {
    pronPerson  :: Person
  , pronNumber  :: NumberGr
  , pronGender  :: Gender
  , pronCase    :: Case
} deriving (Eq,Show)

data Infinitive = Infinitive {
	  infTense :: Tense
  , infVoice :: Voice
} deriving (Eq,Show)

data Adverbial = Adverbial deriving (Eq,Show)

data Conjunction = Conjunction deriving (Eq,Show)

data Particle = Particle deriving (Eq,Show)