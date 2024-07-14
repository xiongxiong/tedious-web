import Tedious.Parser
import Test.Hspec
import Text.Megaparsec (parse)

main :: IO ()
main = hspec tests

tests :: Spec
tests = describe "Tedious.Parser" $ do
  it "pCombo should work" $ do
    parse pCombo "" "Dog"
      `shouldBe` Right (Combo "Dog" Nothing Nothing)
    parse pCombo "" "Dog table \"dog\""
      `shouldBe` Right (Combo "Dog" (Just "dog") Nothing)
    parse pCombo "" "Dog deriving Show Eq"
      `shouldBe` Right (Combo "Dog" Nothing (Just ["Show", "Eq"]))
    parse pCombo "" "Dog table \"dog\" deriving Show Eq"
      `shouldBe` Right (Combo "Dog" (Just "dog") (Just ["Show", "Eq"]))
    parse pCombo "" "Dog deriving Show Eq table \"dog\""
      `shouldBe` Right (Combo "Dog" (Just "dog") (Just ["Show", "Eq"]))
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
  it "pOccur should work" $ do
    parse (pOccur "?") "" ""
      `shouldBe` Right False
    parse (pOccur "?") "" "?"
      `shouldBe` Right True
  it "pFldTypTup should work" $ do
    parse pFldTypTup "" "Text? `bing`"
      `shouldBe` Right ("Text", True, Just "bing")
    parse pFldTypTup "" "Text ? `bing`"
      `shouldBe` Right ("Text", True, Just "bing")
    parse pFldTypTup "" "Text `bing`"
      `shouldBe` Right ("Text", False, Just "bing")
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
    parse pTblFld "" "(Field SqlInt8)"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") False [] Nothing)
    parse pTblFld "" "(Field SqlInt8, Field SqlInt8)"
      `shouldBe` Right (TblFld (TblFldOWR "Field SqlInt8" "Field SqlInt8") False [] Nothing)
    parse pTblFld "" "(\"name\", Field SqlInt8)"
      `shouldBe` Right (TblFld (TblFldONR "name" "Field SqlInt8") False [] Nothing)
    parse pTblFld "" "(\"name\", Field SqlInt8, Field SqlInt8)"
      `shouldBe` Right (TblFld (TblFldONWR "name" "Field SqlInt8" "Field SqlInt8") False [] Nothing)
    parse pTblFld "" "(Field SqlInt8)#"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True [] Nothing)
    parse pTblFld "" "(Field SqlInt8)# default=`3`"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True [] (Just "3"))
    parse pTblFld "" "(Field SqlInt8) #"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True [] Nothing)
    parse pTblFld "" "(Field SqlInt8)# !UniqueOne"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True ["UniqueOne"] Nothing)
    parse pTblFld "" "(Field SqlInt8)# !UniqueOne !UniqueTwo"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True ["UniqueOne", "UniqueTwo"] Nothing)
    parse pTblFld "" "(Field SqlInt8) !UniqueOne !UniqueTwo"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") False ["UniqueOne", "UniqueTwo"] Nothing)
    parse pTblFld "" "(Field SqlInt8)# !UniqueOne !UniqueTwo default=`3`"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") True ["UniqueOne", "UniqueTwo"] (Just "3"))
    parse pTblFld "" "(Field SqlInt8) default=`3`"
      `shouldBe` Right (TblFld (TblFldOR "Field SqlInt8") False [] (Just "3"))
  it "pField should work" $ do
    parse pField "" "firstName `first name` Text `Wang` (\"first_name\", Field SqlText) !UniqueDog !UniqueDogMaster DogA DogC"
      `shouldBe` Right (Field ("firstName", Just "first name") ("Text", False, Just "Wang") (Just (TblFld (TblFldONR "first_name" "Field SqlText") False ["UniqueDog", "UniqueDogMaster"] Nothing)) [("DogA", False), ("DogC", False)])
    parse pField "" "color `dog's color` ((Text, Text, Text)) DogB DogC"
      `shouldBe` Right (Field ("color", Just "dog's color") ("(Text, Text, Text)", False, Nothing) Nothing [("DogB", False), ("DogC", False)])
  it "repPersistTyp should work" $ do
    repPersistTyp (TediousTyp (Combo "Dog" Nothing Nothing) [Field ("name", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'dog'"))) []])
      `shouldBe` ("Dog", TblPrimary ["name"], [TblUnique "One" ["name"]], [("name", "Text", False, Just "'dog'")])
    repPersistTyp
      ( TediousTyp
          (Combo "Dog" Nothing Nothing)
          [ Field ("name", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'dog'"))) [],
            Field ("work", Nothing) ("Text", False, Nothing) (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'work'"))) []
          ]
      )
      `shouldBe` ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"]],
                   [ ("name", "Text", False, Just "'dog'"),
                     ("work", "Text", False, Just "'work'")
                   ]
                 )
  it "strPersistTyp should work" $ do
    strPersistTyp ("Dog", TblPrimary ["name"], [TblUnique "One" ["name"]], [("name", "Text", False, Just "'dog'")])
      `shouldBe` Just "Dog\n\tname Text default='dog'\n\tPrimary name\n\tUniqueOne name\n"
    strPersistTyp ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"]],
                   [ ("name", "Text", False, Just "'dog'"),
                     ("work", "Text", False, Just "'work'")
                   ]
                 )
      `shouldBe` Just "Dog\n\tname Text default='dog'\n\twork Text default='work'\n\tPrimary name work\n\tUniqueOne name work\n"
    strPersistTyp ( "Dog",
                   TblPrimary ["name", "work"],
                   [TblUnique "One" ["name", "work"], TblUnique "Two" ["work", "data"]],
                   [ ("name", "Text", False, Just "'dog'"),
                     ("work", "Text", False, Just "'work'"),
                     ("data", "Text", False, Just "'data'")
                   ]
                 )
      `shouldBe` Just "Dog\n\tname Text default='dog'\n\twork Text default='work'\n\tdata Text default='data'\n\tPrimary name work\n\tUniqueOne name work\n\tUniqueTwo work data\n"
