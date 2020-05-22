module Language.Choucho.ParserSpec where

import Test.Hspec
import Text.Parsec
import Control.Monad.IO.Class

import Language.Choucho.Parser
import Language.Choucho.Types
 
spec :: Spec
spec = do
    describe "talkString" $ do
        it "parse TalkString until end of line" $
            parse talkString "" "hogefuga\n" `shouldBe`
            Right (TalkString "hogefuga")
        it "parse TalkString until left paren" $
            parse talkString "" "hogefuga（" `shouldBe`
            Right (TalkString "hogefuga")
    describe "talk" $ do
        it "parse a talk" $ 
            parse talk "" "＊hoge\nfuga\n\n＊foo" `shouldBe`
            Right (Talk "hoge" [TalkString "fuga",Newline, Newline])
    describe "Question" $ do
        it "parse a questions" $ 
            parse question "" "？hoge\nfuga\nhoge\n＿foo  bar\n＿piyo moge\n＊foo" `shouldBe`
            Right (Question (Just "hoge") "fuga\nhoge\n" [("foo", "bar"), ("piyo", "moge")])
    s <- runIO $ readFile "test/test_dic.txt" 
    describe "dictionary" $ do
        it "parse Dictionary" $
            parse dictionary "" s `shouldBe`
            Right [ChouchoTalk (Talk {tag = "test", content = [TalkString "talk",Newline,Newline]}),ChouchoWords (WordGroup "hoge" ["fuga","foo"]),ChouchoReply (ReplyTalk {keywords = ["my","key","words"], replyContent = [TalkString "hanako",Newline,TalkString "taro",Newline,Newline]}),ChouchoQuestion (Question (Just "title") "some\ntext\n" [("piyo","moge"),("foo","bar")])]
