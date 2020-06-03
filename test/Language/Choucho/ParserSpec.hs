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
            parse talk "" "＊hoge\nfuga\r\n\r\n＊foo" `shouldBe`
            Right (Talk "hoge" [TalkString "fuga"])
        it "contains call" $ 
            parse talk "" "＊hoge\n（fuga）\n＊foo" `shouldBe`
            Right (Talk "hoge" [Call "fuga"])
        it "jumps" $ 
            parse talk "" "＊hoge\n＞fuga\n＊foo" `shouldBe`
            Right (Talk "hoge" [Jump "fuga"])
    describe "Question" $ do
        it "parse a questions" $ 
            parse question "" "？hoge\nfuga\nhoge\n＿foo  bar\n＿piyo moge\n＊foo" `shouldBe`
            Right (Question "hoge" "fuga\nhoge\n" [("foo", "bar"), ("piyo", "moge")])
    s <- runIO $ readFile "test/test_dic.txt" 
    describe "dictionary" $ do
        it "parse Dictionary" $
            parse dictionary "" s `shouldBe`
            Right [ ChouchoTalk (Talk {tag = "test", content = [TalkString "talk"]})
                  , ChouchoWords (WordGroup "hoge" ["fuga","foo"])
                  , ChouchoReply (ReplyTalk {keywords = ["my","key","words"], replyContent = [TalkString "hanako", Call "hoge", TalkString "taro", Jump "title"]})
                  , ChouchoQuestion (Question "title" "some\ntext\n" [("piyo", "moge"),("foo", "bar")])]
