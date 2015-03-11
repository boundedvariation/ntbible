{-# LANGUAGE OverloadedStrings #-}

module Greek.Parsers.DictionaryParser (
    ) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Greek.Dictionary.Types

