import Tedious.Parser
import Test.Hspec
import Text.Megaparsec (parse)

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Tedious.Parser" $ do
  it "pCombo should work" $ do
    parse pCombo "" "Dog"
      `shouldBe` Right (Combo "Dog" Nothing Nothing Nothing)
    parse pCombo "" "Dog table \"dog\""
      `shouldBe` Right (Combo "Dog" (Just "dog") Nothing Nothing)
    parse pCombo "" "Dog deriving Show Eq"
      `shouldBe` Right (Combo "Dog" Nothing (Just ["Show", "Eq"]) Nothing)
    parse pCombo "" "Dog persistLowerCase"
      `shouldBe` Right (Combo "Dog" Nothing Nothing (Just PersistLowerCase))
    parse pCombo "" "Dog table \"dog\" deriving Show Eq persistLowerCase"
      `shouldBe` Right (Combo "Dog" (Just "dog") (Just ["Show", "Eq"]) (Just PersistLowerCase))
    parse pCombo "" "Dog persistLowerCase deriving Show Eq table \"dog\""
      `shouldBe` Right (Combo "Dog" (Just "dog") (Just ["Show", "Eq"]) (Just PersistLowerCase))
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
    parse pTblFld "" "(Field SqlInt8)! !One"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] Nothing))
    parse pTblFld "" "(Field SqlInt8)! !One !Two"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["One", "Two"] Nothing))
    parse pTblFld "" "(Field SqlInt8)! !One !Two default=3"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") True ["One", "Two"] (Just "default=3")))
    parse pTblFld "" "(Field SqlInt8) default=3"
      `shouldBe` Right (Just (TblFld (TblFldOR "Field SqlInt8") False [] (Just "default=3")))
  it "repPersistent should work" $ do
    repPersistent (TtRep (Combo "Dog" Nothing Nothing Nothing) [Field ("name", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "default='dog'"))) []])
      `shouldBe` ("Dog", TblPrimary ["name"], [TblUnique "One" ["name"]], [("name", "Text", False, Just "default='dog'")])
    repPersistent
      ( TtRep
          (Combo "Dog" Nothing Nothing Nothing)
          [ Field ("name", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "default='dog'"))) [],
            Field ("work", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "default='work'"))) []
          ]
      )
      `shouldBe` ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"]],
                   [ ("name", "Text", False, Just "default='dog'"),
                     ("work", "Text", False, Just "default='work'")
                   ]
                 )
  it "strPersistent should work" $ do
    strPersistent ("Dog", TblPrimary ["name"], [TblUnique "One" ["name"]], [("name", "Text", False, Just "default='dog'")])
      `shouldBe` "Dog\n\tname Text default='dog'\n\tPrimary name\n\tUniqueOne name\n"
    strPersistent ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"]],
                   [ ("name", "Text", False, Just "default='dog'"),
                     ("work", "Text", False, Just "default='work'")
                   ]
                 )
      `shouldBe` "Dog\n\tname Text default='dog'\n\twork Text default='work'\n\tPrimary name work\n\tUniqueOne name work\n"
    strPersistent ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"], TblUnique "Two" ["work", "data"]],
                   [ ("name", "Text", False, Just "default='dog'"),
                     ("work", "Text", False, Just "default='work'"),
                     ("data", "Text", False, Just "default='data'")
                   ]
                 )
      `shouldBe` "Dog\n\tname Text default='dog'\n\twork Text default='work'\n\tdata Text default='data'\n\tPrimary name work\n\tUniqueOne name work\n\tUniqueTwo work data\n"
