{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Greek.Dictionary.MorphologyMap (
    createMap
  , createMapFromFile
	) where

import Data.Acid
import Greek.Dictionary.Types
import Greek.Parsers.DictionaryParser
import Control.Applicative
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import qualified Data.Text.IO as I
import qualified Control.Monad.State as S

insertKey :: Key -> Value -> Update Database ()
insertKey key value = do
	Database m <- S.get
	if M.member key m then do
		S.put $ Database $ M.insert key (value:m M.! key) m
	else
		S.put $ Database $ M.insert key [value] m

lookupKey :: Key -> Query Database (Maybe [Value])
lookupKey key = do
	Database m <- ask
	return $ M.lookup key m

deleteKey :: Key -> Update Database ()
deleteKey key = do
	Database m <- S.get
	S.put $ Database $ M.delete key m

allKeys :: Int -> Query Database [(Key,[Value])]
allKeys limit = do
	Database m <- ask
	return $ take limit $ M.toList m

$(makeAcidic ''Database ['insertKey, 'lookupKey, 'allKeys, 'deleteKey])

createMap :: T.Text -> MorphLookup
createMap text = case A.parseOnly morphParser text of
	Left err   -> error err
	Right vals -> process vals M.empty
		where 
			process (m@(MorphEntry lma _ _ _ _):ms) dictMap
				| M.member lma dictMap = 
						process ms $
							M.insert lma (m:(dictMap M.! lma)) dictMap
				| otherwise = process ms $
						M.insert lma [m] dictMap
			process [] dictMap = dictMap

createMapFromFile :: FilePath -> IO MorphLookup
createMapFromFile fp = createMap <$> I.readFile fp