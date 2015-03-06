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
  , Feature
	) where

import qualified Data.Text as T
import qualified Data.Map as M

data NumberGr = Singular 
			  | Plural 
			  | Dual deriving (Eq, Show)

data Gender = Masculine 
			| Feminine 
			| Neuter deriving (Eq, Show)

data Case = Nominative 
		  | Genitive 
		  | Accusative 
		  | Dative 
		  | Vocative deriving (Eq, Show)

type Dialect = T.Text

data Person = First 
			| Second 
			| Third deriving (Eq, Show)

data Tense = Present 
		   | Imperfect 
		   | Aorist 
		   | Perfect
       | Future
		   | Pluperfect deriving (Eq, Show)

data Mood =  Indicative
		   | Imperative
		   | Subjunctive
		   | Optative deriving (Eq, Show)

data Voice = Active
		   | Passive
		   | Middle deriving (Eq, Show)

type Feature = T.Text

type MorphLookup = M.Map T.Text MorphEntry

data MorphEntry = MorphEntry {
	   lemma      :: T.Text
  ,  wordForm   :: T.Text
  ,	 morphology :: Morphology } deriving (Eq,Show)

data Morphology = MorphNoun Noun 
				| MorphVerb Verb 
				| MorphPrep Preposition
				| MorphPart Participle
				| MorphAdj  Adjective
				| MorphAdv  Adverb
        | MorphInf  Infinitive
				| MorphPron Pronoun deriving (Eq,Show)

data Noun = Noun {
	  nounNumber     :: NumberGr
  , nounGender     :: Gender
  , nounCase       :: Case
  , nounDialect    :: [Dialect]
  , nounFeature    :: Feature
} deriving (Eq,Show)

data Verb = Verb {
	  verbPerson  :: Person
  , verbNumber  :: NumberGr
  , verbTense   :: Tense
  , verbMood    :: Mood
  , verbVoice   :: Voice
  , verbDialect :: [Dialect]
  , verbFeature :: Feature
} deriving (Eq,Show)

data Preposition = Preposition Feature deriving (Eq,Show)

data Participle = Participle {
	  partNumber  :: NumberGr
  , partTense   :: Tense
  , partVoice   :: Voice
  , partGender  :: Gender
  , partCase    :: Case
  , partDialect :: [Dialect]
  , partFeature :: Feature
} deriving (Eq,Show)

data Adjective = Adjective {
	  adjNumber  ::  NumberGr
  , adjGender  ::  Gender
  , adjCase    ::  Case
  , adjDialect :: [Dialect]
  , adjFeature :: Feature
} deriving (Eq,Show)

data Adverb = Adverb Feature deriving (Eq,Show)

data Pronoun = Pronoun {
    pronNumber  :: NumberGr
  , pronGender  :: Gender
  , pronCase    :: Case
  , pronDialect :: [Dialect]
  , pronFeature :: Feature
} deriving (Eq,Show)

data Infinitive = Infinitive {
	  infTense :: Tense
  , infVoice :: Voice
  , infDialect :: [Dialect]
  , infFeature :: Feature
} deriving (Eq,Show)