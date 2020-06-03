{-# LANGUAGE TemplateHaskell #-}
module Language.Choucho 
( module Language.Choucho.Types
, module Language.Choucho
) where

import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (isInfixOf)
import System.Random (randomRIO)
import Control.Lens
import Control.Applicative ((<|>))
import Text.Parsec (ParseError)

import Language.Choucho.Types
import Language.Choucho.Parser (parse, dictionary)

type TalkContents = [TalkContent]

emptyChoucho :: Choucho
emptyChoucho = Choucho Map.empty Map.empty Map.empty Map.empty

data Choucho = Choucho 
    { _talk :: Map.Map String [[TalkContent]]
    , _reply :: Map.Map [String] [[TalkContent]]
    , _wordGroup :: Map.Map String [String] 
    , _questions :: Map.Map String [(String, [(String, String)])]
    } deriving (Show, Read)

makeLenses ''Choucho

pickOne :: [a] -> IO a
pickOne ls = do
    ix <- randomRIO (0, length ls -1)
    return $ ls !! ix

getTalk :: Choucho -> Maybe String -> IO [TalkContent]
getTalk c key = do
    t <- getTalkMaybe c key
    case t of
        Just talk -> return talk
        _         -> pickOne randomTalks
    where
        randomTalks = 
            fromMaybe 
                [[TalkString "辞書にランダムトークがありません"]] $
                Map.lookup "" (c^.talk)

getTalkMaybe :: Choucho -> Maybe String -> IO (Maybe [TalkContent])
getTalkMaybe c key = case t of
        Just talks -> Just <$> pickOne talks
        _          -> return Nothing
    where
        key' = fromMaybe "" key
        -- 該当キーのトークを探して、なかったらランダムトークを返す
        -- ランダムトークもなかったらエラーメッセージを返す
        t = Map.lookup key' (c ^. talk)
        l = length t
 

getRandomTalk :: Choucho -> IO [TalkContent]
getRandomTalk c = getTalk c Nothing

-- |
-- getReplyTalk
--
-- 話しかけられた語を含むリプライトークがあったらそれを返す
-- なければランダムトーク
getReplyTalk :: Choucho -> String -> IO [TalkContent]
getReplyTalk c s = 
    case talks of
        Just t -> pickOne t
        _ -> getRandomTalk c
    where
        talks = Map.foldrWithKey f Nothing $ c ^. reply
        f k v a = if any (`isInfixOf`s) k
                  then Just v
                  else a

getQuestion :: Choucho -> String -> IO (Maybe Question)
getQuestion c s = case qs of
                    Just questions -> do
                        (m, ls) <- pickOne questions
                        return . Just $ Question s m ls
                    _              -> return Nothing
    where
        qs = Map.lookup s $ c ^. questions

getWord :: Choucho -> String -> IO (Maybe String)
getWord c s = f $ Map.lookup s $ c ^. wordGroup
    where
        f Nothing = return Nothing 
        f (Just ws) = Just <$> pickOne ws

parseChoucho :: String -> Either ParseError Choucho
parseChoucho s = dictionaryToChoucho <$> parse dictionary "" s

dictionaryToChoucho :: Dictionary -> Choucho
dictionaryToChoucho = foldl f emptyChoucho
    where
        f c (ChouchoTalk t) =
            c & talk %~ Map.insertWith (++) (tag t) [content t]
        f c (ChouchoReply t) = 
            c & reply %~ Map.insertWith (++) (keywords t) [replyContent t]
        f c (ChouchoWords (WordGroup s v)) =
            c & wordGroup %~ Map.insertWith (++) s v
        f c (ChouchoQuestion (Question t m ls)) =
            c & questions %~ Map.insertWith (++) t [(m, ls)]