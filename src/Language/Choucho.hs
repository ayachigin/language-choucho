{-# LANGUAGE TemplateHaskell #-}
module Language.Choucho where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import System.Random (randomRIO)
import Control.Lens
import Control.Applicative ((<|>))

import Language.Choucho.Types
import Language.Choucho.Parser (parse, dictionary)

type TalkContents = [TalkContent]

emptyChoucho :: Choucho
emptyChoucho = Choucho Map.empty Map.empty Map.empty

data Choucho = Choucho 
    { _talk :: Map.Map String [[TalkContent]]
    , _reply :: Map.Map [String] [[TalkContent]]
    , _wordGroup :: Map.Map String [String] 
    } deriving (Show, Read)

makeLenses ''Choucho

getTalk :: Choucho -> Maybe String -> IO [TalkContent]
getTalk c key = do
    ix <- randomRIO (0, l - 1)
    return $ t !! ix
    where
        key' = maybe "" id key
        t = maybe [[TalkString "該当するトークがありません"]] id $
            Map.lookup key' (c ^. talk)
        l = length t

getRandomTalk :: Choucho -> IO [TalkContent]
getRandomTalk c = getTalk c Nothing

-- |
-- getReplyTalk
--
-- 話しかけられた語を含むリプライトークがあったらそれを返す
-- なければランダムトーク
getReplyTalk :: Choucho -> String -> IO [TalkContent]
getReplyTalk c s = do
    case talks of
        Just t -> do
            ix <- randomRIO (0, length t - 1)
            return $ t !! ix
        _ -> getRandomTalk c
    where
        talks = Map.foldrWithKey f Nothing $ c ^. reply
        f k v a = if any (isInfixOf s) k 
                  then Just v
                  else a
        

parseChoucho :: String -> Maybe Choucho
parseChoucho s = 
    case parse dictionary "" s of
        Right dic -> Just $ foldl f emptyChoucho dic
        Left _ -> Nothing
    where
        f c (ChouchoTalk t) =
            c & talk %~ Map.insertWith (++) (tag t) ([content t])
        f c (ChouchoReply t) = 
            c & reply %~ Map.insertWith (++) (keywords t) ([replyContent t])
        f c (ChouchoWords (WordGroup s v)) =
            c & wordGroup %~ Map.insertWith (++) s v

