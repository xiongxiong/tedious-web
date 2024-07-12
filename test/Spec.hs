import Tedious.Parser
import Test.Hspec
import Text.Megaparsec (parse)

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Tedious.Parser" $ do
  it "pTuple should work" $ do
    parse pTuple "" "(Text, Text)"
      `shouldBe` Right "(Text, Text)"
    parse pTuple "" "(Text, Text, Int)"
      `shouldBe` Right "(Text, Text, Int)"
    parse pTuple "" "(Maybe Text, Maybe Text, Maybe Int)"
      `shouldBe` Right "(Maybe Text, Maybe Text, Maybe Int)"
    parse pTuple "" "((Text, Text), (Text, Text))"
      `shouldBe` Right "((Text, Text), (Text, Text))"
    parse pTuple "" "((Text, (Text, Text)), (Text, Text))"
      `shouldBe` Right "((Text, (Text, Text)), (Text, Text))"
  it "pFldTyp should work" $ do
    parse pFldTyp "" "Text"
      `shouldBe` Right "Text"
    parse pFldTyp "" "(Text)"
      `shouldBe` Right "(Text)"
    parse pFldTyp "" "(Text, Int)"
      `shouldBe` Right "(Text, Int)"
    parse pFldTyp "" "(Text, (Text, Int))"
      `shouldBe` Right "(Text, (Text, Int))"
    parse pFldTyp "" "[Text]"
      `shouldBe` Right "[Text]"
    parse pFldTyp "" "Maybe Text"
      `shouldBe` Right "Maybe Text"
    parse pFldTyp "" "Maybe [Text]"
      `shouldBe` Right "Maybe [Text]"
    parse pFldTyp "" "Maybe (Maybe [Int])"
      `shouldBe` Right "Maybe (Maybe [Int])"
  it "pFldTypTup should work" $ do
    parse pFldTypTup "" "Text? `bing`"
      `shouldBe` Right ("Text", True, Just "bing")
    parse pFldTypTup "" "(Text)"
      `shouldBe` Right ("Text", False, Nothing)
    parse pFldTypTup "" "[Text]?"
      `shouldBe` Right ("[Text]", True, Nothing)
    parse pFldTypTup "" "((Text, Text))"
      `shouldBe` Right ("(Text, Text)", False, Nothing)
    parse pFldTypTup "" "((Text, (Text, Maybe Text)))"
      `shouldBe` Right ("(Text, (Text, Maybe Text))", False, Nothing)
    parse pFldTypTup "" "(Maybe Text)"
      `shouldBe` Right ("Maybe Text", False, Nothing)
    parse pFldTypTup "" "(Maybe [Text])"
      `shouldBe` Right ("Maybe [Text]", False, Nothing)
    parse pFldTypTup "" "(Maybe (Maybe [Int]))"
      `shouldBe` Right ("Maybe (Maybe [Int])", False, Nothing)
  it "pTblFld should work" $ do
    parse pTblFld "" ""
      `shouldBe` Right Nothing
    parse pTblFld "" "(Field SqlInt8)"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") False [] Nothing))
    parse pTblFld "" "(Field SqlInt8, Field SqlInt8)"
      `shouldBe` Right (Just (TblFld (TblFldOWR "Field SqlInt8" "Field SqlInt8") False [] Nothing))
    parse pTblFld "" "(\"name\", Field SqlInt8)"
      `shouldBe` Right (Just (TblFld (TblFldONR "name" "Field SqlInt8") False [] Nothing))
    parse pTblFld "" "(\"name\", Field SqlInt8, Field SqlInt8)"
      `shouldBe` Right (Just (TblFld (TblFldONWR "name" "Field SqlInt8" "Field SqlInt8") False [] Nothing))
    parse pTblFld "" "(Field SqlInt8)!"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True [] Nothing))
    parse pTblFld "" "(Field SqlInt8)! default=3"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True [] (Just "default=3")))
    parse pTblFld "" "(Field SqlInt8) !"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True [] Nothing))
    parse pTblFld "" "(Field SqlInt8)! !uniqueOne"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["uniqueOne"] Nothing))
    parse pTblFld "" "(Field SqlInt8)! !uniqueOne !uniqueTwo"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["uniqueOne", "uniqueTwo"] Nothing))
    parse pTblFld "" "(Field SqlInt8)! !uniqueOne !uniqueTwo default=3"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["uniqueOne", "uniqueTwo"] (Just "default=3")))
    parse pTblFld "" "(Field SqlInt8) default=3"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") False [] (Just "default=3")))
  it "repPersistent should work" $ do
    let ttRep = TtRep (Combo "Dog" Nothing Nothing) [Field ("name", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["uniqueOne"] (Just "default='dog'"))) []]
      in repPersistent ttRep
          `shouldBe` Right ()
