module Language.Choucho.Types where

type Name = String

type Comment = String

data Fomura = 
    Number Double
    deriving (Show, Read, Eq)

type Dictionary = [ChouchoTypes]

data ChouchoTypes = 
    ChouchoTalk Talk |
    ChouchoReply ReplyTalk |
    ChouchoWords WordGroup |
    ChouchoButtons Question
    deriving (Show, Read, Eq)

data TalkContent = 
    TalkString String |
    Call String |
    Jump String Fomura |
    Comment Comment |
    Newline
    deriving (Show, Read, Eq)

data Talk = Talk
    { tag :: Name
    , content :: [TalkContent]
    } deriving (Show, Read, Eq)

data ReplyTalk = ReplyTalk 
    { keywords :: [String]
    , replyContent :: [TalkContent]
    } deriving (Show, Read, Eq)

data WordGroup = 
    WordGroup Name [String]
    deriving (Show, Read, Eq)

data Question = 
    Question (Maybe String) String [(String, String)]
    deriving (Show, Read, Eq)