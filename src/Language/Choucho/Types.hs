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
    ChouchoWords WordGroup
    deriving (Show, Read, Eq)

data TalkContent = 
    TalkString String |
    Call String |
    Choices [(String, String)] |
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