{-# LANGUAGE OverloadedStrings #-}
module Language.Choucho.Parser 
    ( parse
    , dictionary
    , talk
    , wordGroup
    , replyTalk
    , talkString
    ) where

import Text.Parsec hiding (newline)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char hiding (newline)
import qualified Text.ParserCombinators.Parsec.Char as P
import Control.Monad (unless)

import Language.Choucho.Types
import Data.Maybe

-- |
-- dictionary
-- 
-- >>> s <- readFile "test/test_dic.txt"
-- >>> parse dictionary "" s
-- Right [ChouchoTalk (Talk {tag = "test", content = [TalkString "talk",Newline,Choices [("choice","bar"),("choice2","baz")]]}),ChouchoReply (ReplyTalk {keywords = ["my","key","words"], replyContent = [TalkString "hanako",Newline,TalkString "taro",Newline]}),ChouchoWords (WordGroup "hoge" ["fuga","foo"])]
dictionary :: Parser Dictionary
dictionary = headerComment >> many chouchoTypes
    where
        chouchoTypes = 
            ChouchoReply <$> try replyTalk 
            <|> ChouchoTalk <$> talk 
            <|> ChouchoWords <$> wordGroup

-- |
-- talk
--
-- >>> parse talk "" "＊hoge\nfuga（foo）"
-- Right (Talk {tag = "hoge", content = [TalkString "fuga",Call "foo"]})
talk :: Parser Talk
talk = do
    sol
    char '＊'
    s <- many1 $ noneOf "\n\r 　"
    many $ oneOf " 　"
    newline
    cs <- talkContents
    return $ Talk s cs

-- |
-- wordGroup
-- 
-- >>> parse wordGroup "" "＠hoge\nfuga\nfoo\nbar\n"
-- Right (WordGroup "hoge" ["fuga","foo","bar"])
wordGroup :: Parser WordGroup
wordGroup = do
    sol
    char '＠'
    s <- many $ noneOf "\n\r 　"
    many $ oneOf " 　"
    newline
    cs <- manyTill anyChar end
    return . WordGroup s . filter (/= "") $ lines cs
    where
        end = eof <|> (sol >> oneOf "＠＊" >> return ()) 
        newline' = newline >> return ()


-- |
-- replyTalk
--
-- >>> parse replyTalk "" "＊「hoge muga\nfoo（bar）"
-- Right (ReplyTalk {keywords = ["hoge","muga"], replyContent = [TalkString "foo",Call "bar"]})
replyTalk :: Parser ReplyTalk
replyTalk = do
    sol
    string "＊「"
    s <- sepEndBy1 keyword (many1 $ oneOf " 　")
    newline
    cs <- talkContents
    return $ ReplyTalk s cs
    where
        keyword = many1 $ noneOf "\n\r 　"            

space' :: Parser ()
space' = oneOf " 　" >> return ()
    
-- |
-- headerComment
--
-- Examples:
-- >>> parse headerComment "" "hoge\n＠"
-- Right (Comment "hoge\n")
headerComment :: Parser TalkContent
headerComment = Comment <$> manyTill anyChar endOfComment 
    where
        endOfComment = do
            sol
            lookAhead $ oneOf "＠＊"
            return ()

-- |
-- talkContents
--
-- >>> parse talkContents "" "hoge（fuga）\n＿hoge fuga\n＿foo bar"
-- Right [TalkString "hoge",Call "fuga",Newline,Choices [("hoge","fuga"),("foo","bar")]]
talkContents :: Parser [TalkContent]
talkContents = manyTill talkContent end
    where
        end = eof <|> (sol >> lookAhead (oneOf "＊＠") >> return ())
        talkContent = 
            lineComment <|>
            choices <|>
            talkString <|>
            call <|>
            newline


-- |
-- sol :: Parser ()
-- matches to start of a line
sol :: Parser ()
sol = do
    pos <- getPosition
    let col = sourceColumn pos
    unless (col == 1) parserZero

-- |
-- lineComment
--
-- >>> parse lineComment "" "＃hoge\r\n"
-- Right (Comment "hoge")
-- >>> parse lineComment "" "＃hoge\n"
-- Right (Comment "hoge")
lineComment :: Parser TalkContent
lineComment = do
    sol
    _ <- oneOf "＃"
    s <- many $ noneOf "\n\r"
    skipMany newline <|> eof
    return . Comment $ s

-- |
-- choices
--
-- >>> parse choices "" "＿hoge fuga\n＿foo bar\n\n"
-- Right (Choices [("hoge","fuga"),("foo","bar")])
choices :: Parser TalkContent
choices = do
    sol
    lookAhead $ char '＿'
    Choices <$> many choices'

choices' :: Parser (String, String)
choices' = do
    sol
    char '＿'
    name <- many1 $ noneOf " 　"
    skipMany1 space
    label <- many1 $ noneOf "\n\r"
    skipMany newline
    return (name, label)

-- |
-- call
--
-- >>> parse call "" "（hoge）"
-- Right (Call "hoge")
call :: Parser TalkContent
call =
    char '（' >>
    Call <$> (many $ noneOf "）") <*
    char '）'

-- |
-- talkString
--
-- >>> parse talkString "" "hoge（"
-- Right (TalkString "hoge")
-- >>> parse talkString "" "fuga\n"
-- Right (TalkString "fuga")
talkString :: Parser TalkContent
talkString = TalkString <$> many1 (noneOf "（\r\n")

-- |
-- newline
--
-- >>> parse newline "" "\n"
-- Right Newline
newline :: Parser TalkContent
newline = do 
    string "\r\n" <|> string "\n"
    return Newline
