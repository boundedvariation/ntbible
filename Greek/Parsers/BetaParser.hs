{-# LANGUAGE OverloadedStrings #-}

module Greek.Parsers.BetaParser (
    parseWord
  , cleanUp
    ) where

import           Data.Attoparsec.Text
import           Control.Applicative
import qualified Data.Text as T
import           Data.Char

cleanUp :: T.Text -> T.Text
cleanUp st = T.filter f $ T.toUpper st
    where f = not . (`elem` validChars)

parseLetter :: Parser Char
parseLetter = parseVowel <|> parseConsonant

parseWord :: Parser String
parseWord = many1 parseLetter

parseConsonant :: Parser Char
parseConsonant = parseCase $ satisfy (`elem` "BCDFGKLMNPQRSTVXYZ")

parseCase :: Parser Char -> Parser Char
parseCase p = (char '*' >> fmap (toUpper . betaConvert) p)
    <|> fmap betaConvert p

betaConvert :: Char -> Char
betaConvert x = case x of
    'A' -> 'α'
    'B' -> 'β'
    'C' -> 'ξ'
    'D' -> 'δ'
    'E' -> 'ε'
    'F' -> 'φ'
    'G' -> 'γ'
    'H' -> 'η'
    'I' -> 'ι'
    'K' -> 'κ'
    'L' -> 'λ'
    'M' -> 'μ'
    'N' -> 'ν'
    'O' -> 'ο'
    'P' -> 'π'
    'Q' -> 'θ'
    'R' -> 'ρ'
    'S' -> 'σ'
    'T' -> 'τ'
    'U' -> 'υ'
    'V' -> 'ϝ'
    'W' -> 'ω'
    'X' -> 'χ'
    'Y' -> 'ψ'
    'Z' -> 'ζ'
    _ -> error "Invalid beta code point."

parseVowel :: Parser Char
parseVowel = parseCase $ choice [
   "A)/|" *> return 'ᾄ'
 , "A)/" *> return 'ἄ'
 , "A)\\|" *> return 'ᾂ'
 , "A)\\" *> return 'ἂ'
 , "A)=|" *> return 'ᾆ'
 , "A)=" *> return 'ἆ'
 , "A)|" *> return 'ᾀ'
 , "A)" *> return 'ἀ'
 , "A(/|" *> return 'ᾅ'
 , "A(/" *> return 'ἅ'
 , "A(\\|" *> return 'ᾃ'
 , "A(\\" *> return 'ἃ'
 , "A(=|" *> return 'ᾇ'
 , "A(=" *> return 'ἇ'
 , "A(|" *> return 'ᾁ'
 , "A(" *> return 'ἁ'
 , "A/|" *> return 'ᾴ'
 , "A/" *> return 'ά'
 , "A\\|" *> return 'ᾲ'
 , "A\\" *> return 'ὰ'
 , "A=|" *> return 'ᾷ'
 , "A=" *> return 'ᾶ'
 , "A|" *> return 'ᾳ'
 , "A" *> return 'α'

 , "E)/" *> return 'ἔ'
 , "E)\\" *> return 'ἒ'
 , "E)" *> return 'ἐ'
 , "E(/" *> return 'ἕ'
 , "E(\\" *> return 'ἓ'
 , "E(" *> return 'ἑ'
 , "E/" *> return 'έ'
 , "E\\" *> return 'ὲ'
 , "E" *> return 'ε'

 , "I)/" *> return 'ἴ'
 , "I)\\" *> return 'ἲ'
 , "I)=" *> return 'ἶ'
 , "I)" *> return 'ἰ'
 , "I(/" *> return 'ἵ'
 , "I(\\" *> return 'ἳ'
 , "I(=" *> return 'ἷ'
 , "I(" *> return 'ἱ'
 , "I/" *> return 'ί'
 , "I\\" *> return 'ὶ'
 , "I=" *> return 'ῖ'
 , "I" *> return 'ι'

 , "O)/" *> return 'ὄ'
 , "O)\\" *> return 'ὂ'
 , "O)" *> return 'ὀ'
 , "O(/" *> return 'ὅ'
 , "O(\\" *> return 'ὃ'
 , "O(" *> return 'ὁ'
 , "O/" *> return 'ό'
 , "O\\" *> return 'ὸ'
 , "O" *> return 'ο'

 , "U)/" *> return 'ὔ'
 , "U)\\" *> return 'ὒ'
 , "U)=" *> return 'ὖ'
 , "U)" *> return 'ὐ'
 , "U(/" *> return 'ὕ'
 , "U(\\" *> return 'ὓ'
 , "U(=" *> return 'ὗ'
 , "U(" *> return 'ὑ'
 , "U/" *> return 'ύ'
 , "U\\" *> return 'ὺ'
 , "U=" *> return 'ῦ'
 , "U" *> return 'υ'

 , "H)/|" *> return 'ᾔ'
 , "H)/" *> return 'ἤ'
 , "H)\\|" *> return 'ᾒ'
 , "H)\\" *> return 'ἢ'
 , "H)=|" *> return 'ᾖ'
 , "H)=" *> return 'ἦ'
 , "H)|" *> return 'ᾐ'
 , "H)" *> return 'ἠ'
 , "H(/|" *> return 'ᾕ'
 , "H(/" *> return 'ἥ'
 , "H(\\|" *> return 'ᾓ'
 , "H(\\" *> return 'ἣ'
 , "H(=|" *> return 'ᾗ'
 , "H(=" *> return 'ἧ'
 , "H(|" *> return 'ᾑ'
 , "H(" *> return 'ἡ'
 , "H/|" *> return 'ᾑ'
 , "H/" *> return 'ῄ'
 , "H\\|" *> return 'ῂ'
 , "H\\" *> return 'ὴ'
 , "H=|" *> return 'ῇ'
 , "H=" *> return 'ῆ'
 , "H|" *> return 'ῃ'
 , "H" *> return 'η'

 , "W)/|" *> return 'ᾤ'
 , "W)/" *> return 'ὤ'
 , "W)\\|" *> return 'ᾢ'
 , "W)\\" *> return 'ὢ'
 , "W)=|" *> return 'ᾦ'
 , "W)=" *> return 'ὦ'
 , "W)|" *> return 'ᾠ'
 , "W)" *> return 'ὠ'
 , "W(/|" *> return 'ᾥ'
 , "W(/" *> return 'ὥ'
 , "W(\\|" *> return 'ᾣ'
 , "W(\\" *> return 'ὣ'
 , "W(=|" *> return 'ᾧ'
 , "W(=" *> return 'ὧ'
 , "W(|" *> return 'ᾡ'
 , "W(" *> return 'ὡ'
 , "W/|" *> return 'ῴ'
 , "W/" *> return 'ώ'
 , "W\\|" *> return 'ῲ'
 , "W\\" *> return 'ὼ'
 , "W=|" *> return 'ῷ'
 , "W=" *> return 'ῶ'
 , "W|" *> return 'ῳ'
 , "W" *> return 'ω'
    ]

validChars :: String
validChars = "*)(/=\\+|.ABCDEFGHIKLMNOPQRSTUVWXYZ,:;-_"