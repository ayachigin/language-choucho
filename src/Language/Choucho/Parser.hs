{-# LANGUAGE OverloadedStrings #-}
module Language.Choucho.Parser 
    ( parse
    , dictionary
    , talk
    , wordGroup
    , question
    , replyTalk
    , talkString
    ) where

import Text.Parsec hiding (newline)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char hiding (newline)
import qualified Text.ParserCombinators.Parsec.Char as P
import Control.Monad (unless, void)

import Language.Choucho.Types
import Data.Maybe

specialSyntax :: Parser ()
specialSyntax = eof <|> (sol >> lookAhead (oneOf "＊＠？") >> return ())

-- |
-- dictionary
-- 
dictionary :: Parser Dictionary
dictionary = headerComment >> many chouchoTypes
    where
        chouchoTypes = 
            ChouchoReply <$> try replyTalk 
            <|> ChouchoTalk <$> talk 
            <|> ChouchoWords <$> wordGroup
            <|> ChouchoQuestion <$> question

-- |
-- talk
--
-- >>> parse talk "" "＊hoge\nfuga（foo）"
-- Right (Talk {tag = "hoge", content = [TalkString "fuga",Call "foo"]})
talk :: Parser Talk
talk = do
    sol
    char '＊'
    s <- many $ noneOf "\n\r 　"
    many $ oneOf " 　"
    newline
    cs <- joinTalkStrings [] <$> talkContents
    return $ Talk s cs

joinTalkStrings :: [TalkContent] -> [TalkContent] -> [TalkContent]
joinTalkStrings acc [] = reverse acc
joinTalkStrings (TalkString s1: acc) (TalkString s2: ls) =
    joinTalkStrings (TalkString (s1<>"\n"<>s2):acc) ls
joinTalkStrings acc (Comment _: ls) =
    joinTalkStrings acc ls
joinTalkStrings acc (l:ls) = joinTalkStrings (l:acc) ls

-- |
-- wordGroup
-- 
-- >>> parse wordGroup "" "＠hoge\nfuga\nfoo\nbar\n＊"
-- Right (WordGroup "hoge" ["fuga","foo","bar"])
wordGroup :: Parser WordGroup
wordGroup = do
    sol
    char '＠'
    s <- many $ noneOf "\n\r 　"
    many $ oneOf " 　"
    newline
    cs <- manyTill anyChar specialSyntax
    return . WordGroup s . filter (/= "") $ lines cs
    where
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
    cs <- joinTalkStrings [] <$> talkContents
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
headerComment = Comment <$> manyTill anyChar specialSyntax

-- |
-- talkContents
--
-- >>> parse talkContents "" "hoge（fuga）foo\n"
-- Right [TalkString "hoge",Call "fuga",TalkString "foo"]
talkContents :: Parser [TalkContent]
talkContents = do
    cs <- manyTill talkContent specialSyntax
    return $ dropTail (==Newline) cs
    where
        dropTail f = reverse . dropWhile f . reverse 
        talkContent = 
            lineComment <|>
            call <|>
            newline <|>
            talkString

-- |
-- question
-- 
-- >>> parse question "" "？title\nsome\ntext\n\n＿hoge    fuga\npiyo   moge\n\n＠"
-- Right (Question "title" "some\ntext\n\n" [("hoge","fuga")])
-- 
question :: Parser Question
question = do
    sol
    char '？'
    title <- many $ noneOf "\n\r 　"
    skipMany $ oneOf " 　"
    newline
    messageText <- manyTill anyChar endOfText
    question <- many1 choices'
    manyTill anyChar specialSyntax
    return $ Question title messageText $ question
    where
        endOfText = (sol >> lookAhead (char '＿')) >> return ()
        


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
    char '＃'
    s <- many $ noneOf "\n\r"
    newline
    return . Comment $ s

-- |
-- choices'
--
-- >>> parse (many1 choices') "" "＿hoge fuga\n＿foo bar\n\n"
-- Right [("hoge","fuga"),("foo","bar")]
choices' :: Parser (String, String)
choices' = do
    sol
    char '＿'
    name <- many1 $ noneOf " 　"
    skipMany1 space
    label <- many1 $ noneOf "\n\r"
    skipMany (newline <|> lineComment)
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
-- >>> parse talkString "" "fuga\n＠"
-- Right (TalkString "fuga")
talkString :: Parser TalkContent
talkString = do
    s <- manyTill anyChar (specialSyntax <|> callStatement <|> comment)
    return . TalkString . reverse . dropWhile (=='\n') . reverse $ s
    where
        callStatement = void (lookAhead $ char '（')
        comment = void (lookAhead lineComment)

-- |
-- newline
--
-- >>> parse newline "" "\n"
-- Right Newline
newline :: Parser TalkContent
newline = do 
    string "\r\n" <|> string "\n"
    return Newline
