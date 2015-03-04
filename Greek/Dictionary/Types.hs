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
  , Number(..)
  , Gender(..)
  , Case(..)
  , Dialect(..)
  , Person(..)
  , Tense(..)
  , Mood(..)
  , Voice(..)
  , MorphLookup
  , Morphology(..)
	) where

import qualified Data.Text as T
import qualified Data.Map as M

data Number = Singular 
			| Plural 
			| Dual deriving (Eq, Show)

data Gender = Masculine 
			| Feminine 
			| Neuter deriving (Eq, Show)

data Case = Nominative 
		  | Genetive 
		  | Accusative 
		  | Dative 
		  | Vocative deriving (Eq, Show)

data Dialect = Attic 
			 | Ionic 
			 | Homeric 
			 | Koine 
			 | Aeolic deriving (Eq, Show)

data Person = First 
			| Second 
			| Third deriving (Eq, Show)

data Tense = Present 
		   | Imperfect 
		   | Aorist 
		   | Perfect 
		   | Pluperfect deriving (Eq, Show)

data Mood =  Indicative
		   | Subjunctive
		   | Optative
		   | Infinitive deriving (Eq, Show)

data Voice = Active
		   | Passive
		   | Middle deriving (Eq, Show)

type Feature = T.Text

type MorphLookup = M.Map T.Text MorphEntry

data MorphEntry = MorphEntry {
	 lemma      :: T.Text
  ,	 morphology :: Morphology }

data Morphology = MorphNoun Noun 
				| MorphVerb Verb 
				| MorphPrep Preposition
				| MorphPart Participle
				| MorphAdj  Adjective
				| MorphAdv  Adverb
				| MorphPron Pronoun deriving (Eq,Show)

data Noun = Noun {
	nounNumber     :: Number
  , nounGender     :: Gender
  , nounCase       :: Case
  , nounDialect    :: [Dialect]
  , nounFeature    :: Feature
} deriving (Eq,Show)

data Verb = Verb {
	verbPerson :: Person
  , verbNumber :: Number
  , verbTense  :: Tense
  , verbMood   :: Mood
  , verbVoice  :: Voice
  , feature    :: Feature
} deriving (Eq,Show)

data Preposition = Preposition Feature deriving (Eq,Show)

data Participle = Participle {
	partNumber :: Number
  , partTense  :: Tense
  , partVoice  :: Voice
  , partGender :: Gender
  , partCase   :: Case
} deriving (Eq,Show)

data Adjective = Adjective {
	adjNumber  ::  Number
  , adjGender  ::  Gender
  , adjCase    ::  Case
  , adjDialect :: [Dialect]
  , adjFeature :: Feature
} deriving (Eq,Show)

data Adverb = Adverb Feature deriving (Eq,Show)

data Pronoun = Pronoun {
	pronPerson  :: Person
  , pronNumber  :: Number
  , pronGender  :: Gender
  , pronCase    :: Case
  , pronFeature :: Feature
} deriving (Eq,Show)