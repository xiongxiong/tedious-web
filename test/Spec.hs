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
    parse pCombo "" "Dog table dog"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoUnQualified "dog")) Nothing)
    parse pCombo "" "Dog table (public, dog)"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoQualified "public" "dog")) Nothing)
    parse pCombo "" "Dog deriving Show Eq"
      `shouldBe` Right (Combo "Dog" Nothing (Just ["Show", "Eq"]))
    parse pCombo "" "Dog table dog deriving Show Eq"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoUnQualified "dog")) (Just ["Show", "Eq"]))
    parse pCombo "" "Dog table (public, dog) deriving Show Eq"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoQualified "public" "dog")) (Just ["Show", "Eq"]))
    parse pCombo "" "Dog table dog deriving Show Eq"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoUnQualified "dog")) (Just ["Show", "Eq"]))
    parse pCombo "" "Dog deriving Show Eq table dog"
      `shouldBe` Right (Combo "Dog" (Just (TblInfoUnQualified "dog")) (Just ["Show", "Eq"]))
  it "pTupleString should work" $ do
    parse pTupleString "" "(Text, Text)"
      `shouldBe` Right "(Text, Text)"
    parse pTupleString "" "(Text, Text, Int)"
      `shouldBe` Right "(Text, Text, Int)"
    parse pTupleString "" "(Maybe Text, Maybe Text, Maybe Int)"
      `shouldBe` Right "(Maybe Text, Maybe Text, Maybe Int)"
    parse pTupleString "" "((Text, Text), (Text, Text))"
      `shouldBe` Right "((Text, Text), (Text, Text))"
    parse pTupleString "" "((Text, (Text, Text)), (Text, Text))"
      `shouldBe` Right "((Text, (Text, Text)), (Text, Text))"
  it "pFldTypS should work" $ do
    parse pFldTypS "" "Text"
      `shouldBe` Right "Text"
    parse pFldTypS "" "(Text)"
      `shouldBe` Right "(Text)"
    parse pFldTypS "" "(Text, Int)"
      `shouldBe` Right "(Text, Int)"
    parse pFldTypS "" "(Text, (Text, Int))"
      `shouldBe` Right "(Text, (Text, Int))"
    parse pFldTypS "" "[Text]"
      `shouldBe` Right "[Text]"
    parse pFldTypS "" "Maybe Text"
      `shouldBe` Right "Maybe Text"
    parse pFldTypS "" "Maybe [Text]"
      `shouldBe` Right "Maybe [Text]"
    parse pFldTypS "" "Maybe (Maybe [Int])"
      `shouldBe` Right "Maybe (Maybe [Int])"
  it "pOccur should work" $ do
    parse (pOccur "?") "" ""
      `shouldBe` Right False
    parse (pOccur "?") "" "?"
      `shouldBe` Right True
  it "pFldTyp should work" $ do
    parse pFldTyp "" "a"
      `shouldBe` Right (FldTypPoly "a")
    parse pFldTyp "" "Text? `bing`"
      `shouldBe` Right (FldTypNormal "Text" True (Just "bing") Nothing)
    parse pFldTyp "" "Text `bing`"
      `shouldBe` Right (FldTypNormal "Text" False (Just "bing") Nothing)
    parse pFldTyp "" "(Text)"
      `shouldBe` Right (FldTypNormal "Text" False Nothing Nothing)
    parse pFldTyp "" "[Text]?"
      `shouldBe` Right (FldTypNormal "[Text]" True Nothing Nothing)
    parse pFldTyp "" "((Text, Text))"
      `shouldBe` Right (FldTypNormal "(Text, Text)" False Nothing Nothing)
    parse pFldTyp "" "((Text, (Text, Maybe Text)))"
      `shouldBe` Right (FldTypNormal "(Text, (Text, Maybe Text))" False Nothing Nothing)
    parse pFldTyp "" "(Maybe Text)"
      `shouldBe` Right (FldTypNormal "Maybe Text" False Nothing Nothing)
    parse pFldTyp "" "(Maybe [Text])"
      `shouldBe` Right (FldTypNormal "Maybe [Text]" False Nothing Nothing)
    parse pFldTyp "" "(Maybe (Maybe [Int]))"
      `shouldBe` Right (FldTypNormal "Maybe (Maybe [Int])" False Nothing Nothing)
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
      `shouldBe` Right (Field ("firstName", Just "first name") (FldTypNormal "Text" False (Just "Wang") (Just (TblFld (TblFldONR "first_name" "Field SqlText") False ["UniqueDog", "UniqueDogMaster"] Nothing))) [ExtTypNormal "DogA" False, ExtTypNormal "DogC" False])
    parse pField "" "color `dog's color` ((Text, Text, Text)) DogB DogC"
      `shouldBe` Right (Field ("color", Just "dog's color") (FldTypNormal "(Text, Text, Text)" False Nothing Nothing) [ExtTypNormal "DogB" False, ExtTypNormal "DogC" False])
  it "repPersistTyp should work" $ do
    repPersistTyp (TediousTyp (Combo "Dog" Nothing Nothing) [Field ("name", Nothing) (FldTypNormal "Text" False Nothing (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'dog'")))) []])
      `shouldBe` ("Dog", TblPrimary ["name"], [TblUnique "One" ["name"]], [("name", "Text", False, Just "'dog'")])
    repPersistTyp
      ( TediousTyp
          (Combo "Dog" Nothing Nothing)
          [ Field ("name", Nothing) (FldTypNormal "Text" False Nothing (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'dog'")))) [],
            Field ("work", Nothing) (FldTypNormal "Text" False Nothing (Just (TblFld (TblFldOR "Field SqlInt8") True ["One"] (Just "'work'")))) []
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
