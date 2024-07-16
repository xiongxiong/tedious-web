{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Tedious.Parser where

import Control.Lens (declareLensesWith, lensRules)
import Control.Lens qualified as L
import Control.Monad (join, when)
import Control.Monad.Cont (MonadCont (..), evalContT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson qualified as A
import Data.Char (isAlphaNum, isLowerCase, isPrint, isUpperCase)
import Data.Default (Default (..))
import Data.Function as F
import Data.Functor (void, (<&>))
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List.Extra (snoc)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.OpenApi (HasExample (..), HasProperties (..), HasRequired (..), HasTitle (..), HasType (..), OpenApiType (..), ToSchema, declareSchemaRef)
import Data.OpenApi qualified as O
import Data.OpenApi.Internal.Schema (named)
import Data.Proxy (Proxy (..))
import Data.Tuple.All (Curry (..), Sel1 (sel1), Sel3 (sel3), Sel4 (sel4), Sel7 (..))
import Data.Void (Void)
import GHC.Generics (Generic (..), Rep)
import Language.Haskell.Meta (parseType)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Opaleye (table, tableField, tableWithSchema)
import Opaleye.Table (Table)
import Tedious.Orphan ()
import Tedious.Util (lowerFirst, toJSONOptions, trimPrefixName_, upperFirst)
import Text.Megaparsec (MonadParsec (takeWhile1P, takeWhileP, try), Parsec, between, empty, errorBundlePretty, optional, parse, (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

type TypName = String

type BasTypName = String

type DevTypName = String

type ExtTypName = String

type FldName = String

type FldLabel = String -- openapi label

type FldTypS = String

type FldSamp = String -- openapi example

type TblSchema = String

type TblName = String

type TblFldOName = String

type TblFldOTypSW = String

type TblFldOTypSR = String

type FldBasIsMaybe = Bool

type FldExtIsMaybe = Bool

type FldExtVar = String

type TblFldIsPrimary = Bool

type TblFldUnique = String

type TblFldDefault = String

type TblUniqueName = String

type TypInfo = (TypName, [DevTypName], RepTediousFields)

type RepTediousFields = [(FldName, Maybe FldLabel, FldTypS, FldBasIsMaybe, Maybe FldSamp, FldExtIsMaybe, Maybe FldExtVar)]

type RepOpaleye = [(TblFldOName, (FldTypS, FldBasIsMaybe), TblFldOTypSW, TblFldOTypSR)]

type RepPersistTyp = (BasTypName, TblPrimary, [TblUnique], [(FldName, FldTypS, FldBasIsMaybe, Maybe TblFldDefault)])

--

data Combo
  = Combo
      BasTypName -- base type name
      (Maybe TblInfo) -- table name
      (Maybe [DevTypName]) -- derivings
  deriving stock (Eq, Show, Generic)

data TblInfo = TblInfoQualified TblSchema TblName | TblInfoUnQualified TblName
  deriving stock (Eq, Show, Generic)

data ComboAttr = ComboTblInfo TblInfo | ComboDevTyp [DevTypName]
  deriving stock (Eq, Show, Generic)

data Field
  = Field
      (FldName, Maybe FldLabel) -- (field name, field label used in openapi schema)
      (FldTypS, FldBasIsMaybe, Maybe FldSamp) -- (type of field on base type, field is maybe or not, example value in openapi schema)
      (Maybe TblFld) -- table field
      [ExtTyp] -- (name of ext type which has this field, the field of the ext type is maybe or not)
  deriving stock (Eq, Show, Generic)

data TblFld = TblFld TblFldOpaleye TblFldIsPrimary [TblFldUnique] (Maybe TblFldDefault)
  deriving stock (Eq, Show, Generic)

data ExtTyp = ExtTypNormal ExtTypName FldExtIsMaybe | ExtTypPoly ExtTypName FldExtVar
  deriving stock (Eq, Show, Generic)

data TblFldOpaleye
  = TblFldOR TblFldOTypSR -- omit field name, write type and read type are same. eg. (Field Text)
  | TblFldOWR TblFldOTypSW TblFldOTypSR -- omit field name, write type and read type are diff. eg. (Maybe (Field Text), Field Text)
  | TblFldONR TblFldOName TblFldOTypSR -- write type and read type are same. eg. ("field_name", Field Text)
  | TblFldONWR TblFldOName TblFldOTypSW TblFldOTypSR -- write type and read type are diff. eg. ("field_name", Maybe (Field Text), Field Text)
  deriving stock (Eq, Show, Generic)

newtype TblPrimary = TblPrimary [FldName]
  deriving stock (Eq, Show, Generic)

data TblUnique = TblUnique TblUniqueName [FldName]
  deriving stock (Eq, Show, Generic)

data TediousTyp = TediousTyp Combo [Field]
  deriving stock (Eq, Show, Generic)

--

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = MCL.skipLineComment "--"

sc :: Parser ()
sc = MCL.space (void $ M.some (MC.char ' ' <|> MC.char '\t')) lineComment empty

scn :: Parser ()
scn = MCL.space MC.space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme sc

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

pName :: Parser String
pName = lexeme pName_

pName_ :: Parser String
pName_ = takeWhile1P Nothing isNameChar

pNameLower :: Parser String
pNameLower = lexeme pNameLower_

pNameLower_ :: Parser String
pNameLower_ = (<>) <$> takeWhile1P Nothing isLowerCase <*> takeWhileP Nothing isNameChar

pNameUpper :: Parser String
pNameUpper = lexeme pNameUpper_

pNameUpper_ :: Parser String
pNameUpper_ = (<>) <$> takeWhile1P Nothing isUpperCase <*> takeWhileP Nothing isNameChar

string :: String -> Parser String
string = lexeme . MC.string

symbol :: String -> Parser String
symbol = MCL.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parens_ :: Parser a -> Parser a
parens_ = between (symbol "(") (MC.char ')')

parens' :: Parser String -> Parser String
parens' p = join <$> sequence [pure "(", between (symbol "(") (symbol ")") p, pure ")"]

parens'_ :: Parser String -> Parser String
parens'_ p = join <$> sequence [pure "(", between (symbol "(") (MC.char ')') p, pure ")"]

pTupleString :: Parser String
pTupleString =
  join
    <$> sequence
      [ pure "(",
        between
          (symbol "(")
          (symbol ")")
          ( (<>)
              <$> ((unwords <$> M.some pNameUpper) <|> pTupleString)
              <*> (concat <$> M.some (unwords <$> (((<>) . pure <$> symbol ",") <*> (M.some pNameUpper <|> (pure <$> pTupleString)))))
          ),
        pure ")"
      ]

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

brackets_ :: Parser a -> Parser a
brackets_ = between (symbol "[") (MC.char ']')

brackets' :: Parser String -> Parser String
brackets' p = join <$> sequence [pure "[", between (symbol "[") (symbol "]") p, pure "]"]

brackets'_ :: Parser String -> Parser String
brackets'_ p = join <$> sequence [pure "[", between (symbol "[") (MC.char ']') p, pure "]"]

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

quotes_ :: Parser a -> Parser a
quotes_ = between (symbol "\"") (MC.char '\"')

backQuotes :: Parser a -> Parser a
backQuotes = between (symbol "`") (symbol "`")

backQuotes_ :: Parser a -> Parser a
backQuotes_ = between (symbol "`") (MC.char '`')

backQuoteString :: Parser String
backQuoteString = lexeme . backQuotes $ takeWhileP Nothing (\c -> isPrint c && c /= '`')

pTblInfo :: Parser TblInfo
pTblInfo = string "table" *> (parens (TblInfoQualified <$> pName <*> (symbol "," *> pName)) <|> (TblInfoUnQualified <$> pName))

pDevTyp :: Parser [DevTypName]
pDevTyp = string "deriving" *> lexeme (M.some pNameUpper)

pComboAttr :: Parser ComboAttr
pComboAttr = (ComboTblInfo <$> pTblInfo) <|> (ComboDevTyp <$> pDevTyp)

pComboAttrs :: Parser (Maybe TblInfo, Maybe [DevTypName])
pComboAttrs = do
  attrs <- M.many pComboAttr
  let attrTblName = listToMaybe $ mapMaybe (\case (ComboTblInfo tblInfo) -> Just tblInfo; _ -> Nothing) attrs
  let attrDevTyp = listToMaybe $ mapMaybe (\case (ComboDevTyp devTyps) -> Just devTyps; _ -> Nothing) attrs
  pure (attrTblName, attrDevTyp)

pCombo :: Parser Combo
pCombo = uncurryN . Combo <$> pNameUpper <*> pComboAttrs

pFldName :: Parser FldName
pFldName = pNameLower

pFldTitle :: Parser FldLabel
pFldTitle = backQuoteString

pFldNameAndTitle :: Parser (FldName, Maybe FldLabel)
pFldNameAndTitle = (,) <$> pFldName <*> optional pFldTitle

pFldTyp :: Parser FldTypS
pFldTyp =
  try (unwords <$> (((<>) . pure . unwords <$> M.some pNameUpper) <*> (pure . unwords <$> M.some pFldTyp)))
    <|> try (parens' pFldTyp)
    <|> try (brackets' pFldTyp)
    <|> try pTupleString
    <|> (unwords <$> M.some pNameUpper)

pFldSamp :: Parser FldSamp
pFldSamp = backQuoteString

pOccur :: String -> Parser Bool
pOccur s = lexeme $ (True <$ symbol s) <|> pure False

pFldTypTup :: Parser (FldTypS, FldBasIsMaybe, Maybe FldSamp)
pFldTypTup = do
  _fldTypS <- pNameUpper_ <|> parens_ pFldTyp <|> brackets'_ pFldTyp
  _fldBasIsM <- pOccur "?"
  _mFldSamp <- optional pFldSamp
  pure (_fldTypS, _fldBasIsM, _mFldSamp)

pTblFldUnique :: Parser TblFldUnique
pTblFldUnique = lexeme $ MC.char '!' *> pNameUpper

pTblFldDefault :: Parser String
pTblFldDefault = lexeme $ (<>) <$> MC.string "default=" *> backQuoteString

pTblFld :: Parser TblFld
pTblFld =
  pFldP (parens_ (TblFldOR <$> pFldO))
    <|> pFldP (parens_ (TblFldOWR <$> (pFldM <|> pFldO) <*> (symbol "," *> pFldO)))
    <|> pFldP (parens_ (TblFldONR <$> quotes pName <*> (symbol "," *> (pFldM <|> pFldO))))
    <|> pFldP (parens_ (TblFldONWR <$> quotes pName <*> (symbol "," *> (pFldM <|> pFldO)) <*> (symbol "," *> pFldO)))
  where
    pFldO = unwords <$> (((<>) . pure <$> (symbol "FieldNullable" <|> symbol "Field")) <*> (pure <$> pNameUpper))
    pFldM = unwords <$> ((<>) . pure <$> symbol "Maybe" <*> (pure <$> parens' pFldO))
    pFldP p = try (TblFld <$> p <*> pOccur "#" <*> M.many pTblFldUnique <*> optional pTblFldDefault)

pExtName :: Parser ExtTyp
pExtName = try (ExtTypPoly <$> pNameUpper_ <*> (MC.char ':' *> pNameLower)) <|> (ExtTypNormal <$> pNameUpper_ <*> pOccur "?")

pField :: Parser Field
pField = Field <$> pFldNameAndTitle <*> pFldTypTup <*> optional pTblFld <*> M.many pExtName

pTediousTyp :: Parser TediousTyp
pTediousTyp = MCL.indentBlock scn $ do
  combo <- pCombo
  return $ MCL.IndentSome Nothing (return . TediousTyp combo) pField

pTediousTyps :: Parser [TediousTyp]
pTediousTyps = M.many pTediousTyp

--

devIns :: [String]
devIns = ["Eq", "Show", "Generic"]

noDevIns :: [String]
noDevIns = ["Default", "ToJSON", "FromJSON", "ToSchema"]

repTediousTyp :: TediousTyp -> (HM.HashMap TypName RepTediousFields, [DevTypName])
repTediousTyp (TediousTyp (Combo basTypName _ devs) flds) =
  let hm = defBase basTypName (flds <&> (\(Field tupleNameTitle tup _ _) -> (tupleNameTitle, tup))) HM.empty
   in (defExts flds hm, devIns <> filter (`notElem` (devIns <> noDevIns)) (fromMaybe empty devs))
  where
    defBase _basTypName tuples = HM.insert _basTypName (tuples <&> (\((_fldName, _mFldTitle), (_fldTypS, _maybeFldBas, _mFldSamp)) -> (_fldName, _mFldTitle, _fldTypS, _maybeFldBas, _mFldSamp, False, Nothing)))
    defExts [] m = m
    defExts (Field _ _ _ [] : ds) m = defExts ds m
    defExts (Field (_fldName, _mFldTitle) (_fldTypS, _fldBasIsM, _mFldSamp) _mTblFld (extTyp : extTyps) : _flds) m =
      let (_extTypName, _fldExtIsM, _mFldExtVar) = procExtTyp extTyp
          m' = HM.insert _extTypName (snoc (HM.lookupDefault [] _extTypName m) (_fldName, _mFldTitle, _fldTypS, _fldBasIsM, _mFldSamp, _fldExtIsM, _mFldExtVar)) m
       in defExts (Field (_fldName, _mFldTitle) (_fldTypS, _fldBasIsM, _mFldSamp) _mTblFld extTyps : _flds) m'
    procExtTyp extTyp = case extTyp of
      ExtTypNormal _extTypName _fldExtIsM -> (_extTypName, _fldExtIsM, Nothing)
      ExtTypPoly _extTypName _fldExtVar -> (_extTypName, False, Just _fldExtVar)

repOpaleye :: [Field] -> RepOpaleye
repOpaleye = mapMaybe go
  where
    go (Field (_fldName, _) (_fldTypS, _fldBasIsM, _mFldSamp) mTblFld _) = case mTblFld of
      Nothing -> Nothing
      Just (TblFld _tblFld _ _ _) -> case _tblFld of
        TblFldOR _tblFldTypSR -> Just (_fldName, (_fldTypS, _fldBasIsM), _tblFldTypSR, _tblFldTypSR)
        TblFldOWR _tblFldTypSW _tblFldTypSR -> Just (_fldName, (_fldTypS, _fldBasIsM), _tblFldTypSW, _tblFldTypSR)
        TblFldONR _tblFldName _tblFldTypSR -> Just (_tblFldName, (_fldTypS, _fldBasIsM), _tblFldTypSR, _tblFldTypSR)
        TblFldONWR _tblFldName _tblFldTypSW _tblFldTypSR -> Just (_tblFldName, (_fldTypS, _fldBasIsM), _tblFldTypSW, _tblFldTypSR)

repPersistTyp :: TediousTyp -> RepPersistTyp
repPersistTyp (TediousTyp (Combo basTypName _ _) flds) =
  let primaryCons = TblPrimary (genPrimaryCons flds)
      uniqueCons = genUniqueCons flds []
      persistFlds = mapMaybe genPersistFld flds
   in (basTypName, primaryCons, uniqueCons, persistFlds)
  where
    genPrimaryCons = mapMaybe extractPrimary
    extractPrimary (Field (_fldName, _) (_fldTypS, _fldBasIsM, _) mTblFld _) = case mTblFld of
      Nothing -> Nothing
      Just (TblFld _ isPrimary _ _def) -> if isPrimary then Just _fldName else Nothing
    genUniqueCons [] uCons = uCons
    genUniqueCons ((Field (_fldName, _) _ mTblFld _) : _flds) uCons = case mTblFld of
      Nothing -> genUniqueCons _flds uCons
      Just (TblFld _ _ uNames _) -> genUniqueCons _flds (extractUnique _fldName uNames uCons)
    extractUnique _fldName [] uCons = uCons
    extractUnique _fldName (uName : uNames) uCons = extractUnique _fldName uNames (extractUniqueOne _fldName uName uCons [])
    extractUniqueOne _fldName uName [] uCons = reverse (TblUnique uName [_fldName] : uCons)
    extractUniqueOne _fldName uName (uCon@(TblUnique uConName uConFlds) : uCons_) uCons =
      if uName == uConName
        then reverse uCons_ <> (TblUnique uConName (snoc uConFlds _fldName) : uCons)
        else extractUniqueOne _fldName uName uCons_ (uCon : uCons)
    genPersistFld (Field (_fldName, _) (_fldTypS, _fldBasIsM, _) mTblFld _) = case mTblFld of
      Nothing -> Nothing
      Just (TblFld _ _ _ _def) ->
        if _fldName == "id" && _fldTypS == "Int64"
          then Nothing
          else Just (_fldName, wrapperParens _fldTypS, _fldBasIsM, _def)
    wrapperParens s =
      if length (words s) > 1
        then "(" <> s <> ")"
        else s

strPersistTyp :: RepPersistTyp -> Maybe String
strPersistTyp (basTypName, TblPrimary pNames, uCons, tblFlds) =
  let primaryLine = if null pNames then Nothing else Just . unwords $ "Primary" : pNames
      uniqueLines = uCons <&> (\(TblUnique uConName uConFlds) -> unwords $ ("Unique" <> uConName) : uConFlds)
      tblFldLines =
        tblFlds
          <&> ( \(fldName, fldTypS, fldBasIsMaybe, mTblFldDef) ->
                  unwords . catMaybes $ [Just fldName, Just fldTypS, if fldBasIsMaybe then Just "Maybe" else Nothing, ("default=" <>) <$> mTblFldDef]
              )
   in if null tblFldLines
        then Nothing
        else Just . unlines $ pure basTypName <> (indent 1 <$> catMaybes ((Just <$> tblFldLines) <> pure primaryLine <> (Just <$> uniqueLines)))
  where
    indent n s = replicate n '\t' <> s

tedious :: QuasiQuoter
tedious =
  QuasiQuoter
    { quoteExp = error "tedious cannot be used as exp",
      quotePat = error "tedious cannot be used as pat",
      quoteType = error "tedious cannot be used as type",
      quoteDec = decTedious
    }

decTedious ::
  String ->
  Q [Dec]
decTedious str = do
  let tts = case parse pTediousTyps "" str of
        Left b -> error ("parse pTedious : " <> errorBundlePretty b)
        Right tts_ -> tts_
  let repTts = tts <&> repTediousTyp
  let reps = repTts >>= (\(m, devs) -> HM.toList m <&> (\(n, flds) -> (n, devs, flds)))
  tediousTypDecs <- join <$> mapM repDec reps
  opaleyeDecs <- join <$> mapM decOpaleye tts
  persistDecs <- decPersist tts
  return (tediousTypDecs <> opaleyeDecs <> persistDecs)
  where
    repDec :: TypInfo -> Q [Dec]
    repDec typInfo = do
      _decBasic <- decBasic typInfo
      _decStandaloneDerivs <- decStandaloneDerivs typInfo
      _decToSchema <- decToSchema typInfo
      _decJSON <- decJSON typInfo
      decLens <- dropWhile isDataD <$> declareLensesWith lensRules (pure _decBasic)
      pure $ _decBasic <> _decStandaloneDerivs <> _decJSON <> _decToSchema <> decLens

decBasic :: TypInfo -> Q [Dec]
decBasic (typName, _, flds) = do
  let name = mkName typName
  let vbs =
        flds
          <&> ( \(_fldName, _, _fldTypS, _fldBasIsM, _mFldSamp, _fldExtIsM, _mFldExtVar) ->
                  let _fldTyp = case _mFldExtVar of
                        Nothing -> pure $ strToTyp _fldTypS (_fldBasIsM || _fldExtIsM)
                        Just _fldExtVar -> varT (mkName _fldExtVar)
                   in varBangType (mkName $ "_" <> lowerFirst typName <> upperFirst _fldName) (bangType (bang noSourceUnpackedness noSourceStrictness) _fldTyp)
              )
  let bndrs = [plainTV (mkName _fldExtVar) | _fldExtVar <- mapMaybe sel7 flds]
  let dec =
        if length vbs == 1
          then
            newtypeD mempty name bndrs Nothing (recC name vbs) []
          else
            dataD mempty name bndrs Nothing [recC name vbs] []
  pure <$> dec

decStandaloneDerivs :: TypInfo -> Q [Dec]
decStandaloneDerivs (typName, _devClsNames, flds) =
  return $
    _devClsNames
      <&> ( \_devClsName ->
              let _fldExtVars = mapMaybe sel7 flds
                  preds = [AppT (ConT (mkName _devClsName)) (VarT (mkName _fldExtVar)) | _fldExtVar <- _fldExtVars]
               in StandaloneDerivD Nothing preds (AppT (ConT (mkName _devClsName)) (typWithVars typName _fldExtVars))
          )

decJSON :: TypInfo -> Q [Dec]
decJSON (typName, _devClsNames, flds) = do
  let _fldExtVars = mapMaybe sel7 flds
  decToJSON <- do
    eToJ <- [|genericToJSON toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
    let fToJ = FunD 'A.toJSON [Clause [] (NormalB eToJ) []]
    eToE <- [|genericToEncoding toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
    let fToE = FunD 'A.toEncoding [Clause [] (NormalB eToE) []]
    let preds =
          [ [ AppT (ConT ''Generic) (VarT (mkName _fldExtVar)),
              AppT (AppT (AppT (ConT ''A.GToJSON') (ConT ''A.Value)) (ConT ''A.Zero)) (AppT (ConT ''Rep) (VarT (mkName _fldExtVar))),
              AppT (AppT (AppT (ConT ''A.GToJSON') (ConT ''A.Encoding)) (ConT ''A.Zero)) (AppT (ConT ''Rep) (VarT (mkName _fldExtVar))),
              AppT (ConT ''ToJSON) (VarT (mkName _fldExtVar))
            ]
            | _fldExtVar <- _fldExtVars
          ]
    return $ InstanceD Nothing (join preds) (AppT (ConT ''ToJSON) (typWithVars typName _fldExtVars)) [fToJ, fToE]
  decFromJSON <- do
    e <- [|genericParseJSON toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
    let preds =
          [ [ AppT (ConT ''Generic) (VarT (mkName _fldExtVar)),
              AppT (AppT (ConT ''A.GFromJSON) (ConT ''A.Zero)) (AppT (ConT ''Rep) (VarT (mkName _fldExtVar))),
              AppT (ConT ''FromJSON) (VarT (mkName _fldExtVar))
            ]
            | _fldExtVar <- _fldExtVars
          ]
    return $ InstanceD Nothing (join preds) (AppT (ConT ''FromJSON) (typWithVars typName _fldExtVars)) [FunD 'A.parseJSON [Clause [] (NormalB e) []]]
  pure [decToJSON, decFromJSON]

decToSchema :: TypInfo -> Q [Dec]
decToSchema (typName, _devClsNames, flds) = do
  let name = mkName typName
  let _fldExtVars = mapMaybe sel7 flds
  let preds = [AppT (ConT ''ToSchema) (VarT (mkName _fldExtVar)) | _fldExtVar <- _fldExtVars]
  let tuples =
        ( \(_fldName, _mFldTitle, _fldTypS, _fldBasIsM, _mFldSamp, _fldExtIsM, _mFldExtVar) -> do
            let _fldTyp = case _mFldExtVar of
                  Nothing -> strToTyp _fldTypS (_fldBasIsM || _fldExtIsM)
                  Just _fldExtVar -> VarT (mkName _fldExtVar)
            let sigProxy = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) _fldTyp)
            (_fldName, _mFldTitle, _fldTyp, isMaybeTyp _fldTyp, _mFldSamp, AppE (VarE 'declareSchemaRef) sigProxy)
        )
          <$> flds
  let bindStmts = tuples <&> (\(_fldName, _, _, _, _, _schemaRefExp) -> bindS (varP (mkName $ fldSchemaName _fldName)) (pure _schemaRefExp))
  let u1 = uInfixE (varE 'type_) (varE '(L.?~)) (conE 'OpenApiObject)
  let u2 =
        uInfixE
          (varE 'properties)
          (varE '(L..~))
          ( appE
              (varE 'IHM.fromList)
              ( listE $
                  tuples
                    <&> ( \(_fldName, _mFldTitle, _, _, _, _fldBasIsM) ->
                            tupE [stringE _fldName, uInfixE (varE (mkName $ fldSchemaName _fldName)) (varE '(<&>)) (uInfixE (varE 'title) (varE '(L..~)) [|_mFldTitle|])]
                        )
              )
          )
  let u3 = uInfixE (varE 'required) (varE '(L..~)) (listE $ stringE . sel1 <$> filter (not . sel4) tuples)
  let sampTup =
        tupE $
          tuples
            <&> ( \(_, _, _fldTyp, _isMaybeFldTyp, _mFldSamp, _) ->
                    case _mFldSamp of
                      Nothing -> sigE (varE 'def) (pure _fldTyp)
                      Just _fldSamp -> sigE (appE (varE 'read) (stringE _fldSamp)) (pure _fldTyp)
                )
  let samp = appE (appE (varE 'uncurryN) (conE name)) sampTup
  let u4 = uInfixE (varE 'example) (varE '(L.?~)) (appE (varE 'toJSON) samp)
  let u = if null _fldExtVars
            then uInfixE (uInfixE (uInfixE (uInfixE (varE 'mempty) (varE '(F.&)) u1) (varE '(F.&)) u2) (varE '(F.&)) u3) (varE '(F.&)) u4
            else uInfixE (uInfixE (uInfixE (varE 'mempty) (varE '(F.&)) u1) (varE '(F.&)) u2) (varE '(F.&)) u3
  let pureStmt =
        noBindS
          ( appE
              (varE 'return)
              ( appE
                  (appE (varE 'named) (stringE typName))
                  u
              )
          )
  pure <$> instanceD (pure preds) (appT (conT ''ToSchema) (pure (typWithVars typName _fldExtVars))) [funD 'O.declareNamedSchema [clause [wildP] (normalB (doE $ bindStmts <> [pureStmt])) []]]

decOpaleye :: TediousTyp -> Q [Dec]
decOpaleye (TediousTyp (Combo basTypName mTblInfo _) flds) = evalContT $ do
  callCC $ \exit -> do
    let funbasTypName = lowerFirst basTypName <> "Table"
    let tblFlds = repOpaleye flds
    when (null tblFlds) $ exit mempty
    let wTyps = (`strToTyp` False) . sel3 <$> tblFlds
    let wFlds = genSigFields wTyps Nothing
    let vTyps = (`strToTyp` False) . sel4 <$> tblFlds
    let vFlds = genSigFields vTyps Nothing
    sig <- lift $ sigD (mkName funbasTypName) (appT (appT (conT ''Table) wFlds) vFlds)
    let nFlds = sel1 <$> tblFlds
    let eFlds = appE (varE 'tableField) . litE . stringL <$> nFlds
    fun <-
      lift $
        funD
          (mkName funbasTypName)
          [ clause
              []
              ( normalB
                  ( appE
                      (appTable basTypName mTblInfo)
                      (appE (varE (mkName $ "p" <> (show . length $ nFlds))) (genFunFields eFlds))
                  )
              )
              []
          ]
    return [sig, fun]
  where
    genSigFields (t : ts) Nothing = genSigFields ts (Just $ if null ts then t else AppT (TupleT $ length ts + 1) t)
    genSigFields (t : ts) (Just t') = genSigFields ts (Just (AppT t' t))
    genSigFields [] (Just t') = return t'
    genSigFields [] Nothing = return $ ConT ''()
    genFunFields es | length es > 1 = tupE es
    genFunFields [e] = parensE e
    genFunFields _ = fail "makeTable : empty flds"
    appTable basTypName_ mTblInfo_ = case mTblInfo_ of
      Nothing -> appE (varE 'table) (litE (stringL basTypName_))
      Just (TblInfoQualified tblSchema_ tblName_) -> appE (appE (varE 'tableWithSchema) (litE (stringL tblSchema_))) (litE (stringL tblName_))
      Just (TblInfoUnQualified tblName_) -> appE (varE 'table) (litE (stringL tblName_))

decPersist :: [TediousTyp] -> Q [Dec]
decPersist tts = do
  let unboundEntityDefs = unlines $ mapMaybe (strPersistTyp . repPersistTyp) tts
  let name_ = mkName "tediousPersistString"
  sigD_ <- sigD name_ (conT ''String)
  valD_ <- valD (varP name_) (normalB (litE (stringL unboundEntityDefs))) []
  pure [sigD_, valD_]

-- decPersist :: Maybe MigrateName -> Maybe PersistSetting -> String -> Q [Dec]
-- decPersist mMn (Just PersistUpperCase) s = share [mkPersist sqlSettings, mkMigrate (fromMaybe "migrateAll" mMn)] $(persistUpperCase s)
-- decPersist mMn _ s = share [mkPersist sqlSettings, mkMigrate (fromMaybe "migrateAll" mMn)] $(persistUpperCase s)

strToTyp :: String -> Bool -> Type
strToTyp s m =
  let ot = either (error "decTedious: cannot parse field type") id (parseType s)
   in if m then AppT (ConT ''Maybe) ot else ot

typWithVars :: TypName -> [FldExtVar] -> Type
typWithVars name = go (ConT (mkName name))
  where
    go t [] = t
    go t (var:vars) = go (AppT t (VarT (mkName var))) vars

isMaybeTyp :: Type -> Bool
isMaybeTyp (AppT (ConT c) _)
  | c == ''Maybe = True
  | otherwise = False
isMaybeTyp _ = False

isDataD :: Dec -> Bool
isDataD DataD {} = True
isDataD NewtypeD {} = True
isDataD _ = False

fldSchemaName :: FldName -> String
fldSchemaName = ("schema" <>) . upperFirst

--

{-
> :set -XTemplateHaskell
> $(stringE . show =<< reify ''Hello)
> parseTest pName "Hello"
-}
