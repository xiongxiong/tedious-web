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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi (HasExample (..), HasProperties (..), HasRequired (..), HasTitle (..), HasType (..), OpenApiType (..), ToSchema, declareSchemaRef)
import Data.OpenApi qualified as O
import Data.OpenApi.Internal.Schema (named)
import Data.Proxy (Proxy (..))
import Data.Tuple.All (Curry (..), Sel1 (sel1), Sel3 (sel3), Sel4 (sel4))
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Meta (parseType)
import Language.Haskell.TH (Body (NormalB), Clause (Clause), Dec (..), DerivStrategy (..), Exp (..), Pat (WildP), Q, Type (AppT, ConT, TupleT), appE, appT, bang, bangType, bindS, clause, conE, conT, dataD, derivClause, doE, funD, listE, litE, mkName, newtypeD, noBindS, noSourceStrictness, noSourceUnpackedness, normalB, parensE, recC, sigD, sigE, stringE, stringL, tupE, uInfixE, varBangType, varE, varP)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Opaleye (table, tableField)
import Opaleye.Table (Table)
import Tedious.Orphan ()
import Tedious.Util (lowerFirst, toJSONOptions, trimPrefixName_, upperFirst)
import Text.Megaparsec (MonadParsec (takeWhile1P, takeWhileP, try), Parsec, between, empty, errorBundlePretty, optional, parse, (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

type TypName = String

type BasTypName = String

type DevClsName = String

type ExtTypName = String

type FldName = String

type FldTitle = String

type FldTypS = String

type FldSamp = String

type TblName = String

type TblFldName = String

type TblFldTypSW = String

type TblFldTypSR = String

type MaybeFldBas = Bool

type MaybeFldExt = Bool

--

data Combo
  = Combo
      BasTypName -- base type name
      (Maybe TblName) -- table name
      (Maybe [DevClsName]) -- derivings
  deriving stock (Eq, Show, Generic)

data Field
  = Field
      (FldName, Maybe FldTitle) -- (field name, field title used in openapi schema)
      (FldTypS, MaybeFldBas, Maybe FldSamp) -- (type of field on base type, field is maybe or not, example value in openapi schema)
      (Maybe TblFld) -- table field
      [(ExtTypName, MaybeFldExt)] -- (name of ext type which has this field, field of this type is maybe or not)
  deriving stock (Eq, Show, Generic)

data TblFld
  = TblFldR TblFldTypSR -- omit field name, write type and read type are same. eg. (Field Text)
  | TblFldWR TblFldTypSW TblFldTypSR -- omit field name, write type and read type are diff. eg. (Maybe (Field Text), Field Text)
  | TblFldNR TblFldName TblFldTypSR -- write type and read type are same. eg. ("field_name", Field Text)
  | TblFldNWR TblFldName TblFldTypSW TblFldTypSR -- write type and read type are diff. eg. ("field_name", Maybe (Field Text), Field Text)
  deriving stock (Eq, Show, Generic)

data TtRep = TtRep Combo [Field] deriving stock (Eq, Show, Generic)

--

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = MCL.skipLineComment "--"

scn :: Parser ()
scn = MCL.space MC.space1 lineComment empty

sc :: Parser ()
sc = MCL.space (void $ M.some (MC.char ' ' <|> MC.char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme sc

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

pName :: Parser String
pName = lexeme $ takeWhile1P Nothing isNameChar

pNameLower :: Parser String
pNameLower = lexeme ((<>) <$> takeWhile1P Nothing isLowerCase <*> takeWhileP Nothing isNameChar)

pNameUpper :: Parser String
pNameUpper = lexeme ((<>) <$> takeWhile1P Nothing isUpperCase <*> takeWhileP Nothing isNameChar)

pString :: Parser (Maybe String)
pString = optional . lexeme . backQuotes $ takeWhileP Nothing (\c -> isPrint c && c /= '`')

symbol :: String -> Parser String
symbol = MCL.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parens' :: Parser String -> Parser String
parens' p = join <$> sequence [pure "(", between (symbol "(") (symbol ")") p, pure ")"]

pTuple :: Parser String
pTuple = join <$> sequence [pure "(", between (symbol "(") (symbol ")") ((<>) <$> ((unwords <$> M.some pNameUpper) <|> pTuple) <*> (concat <$> M.some (unwords <$> (((<>) . pure <$> symbol ",") <*> (M.some pNameUpper <|> (pure <$> pTuple)))))), pure ")"]

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

brackets' :: Parser String -> Parser String
brackets' p = join <$> sequence [pure "[", between (symbol "[") (symbol "]") p, pure "]"]

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

backQuotes :: Parser a -> Parser a
backQuotes = between (symbol "`") (symbol "`")

pTbl :: Parser (Maybe TblName)
pTbl = optional $ lexeme (MC.string "table") *> quotes pName

pDev :: Parser (Maybe [DevClsName])
pDev = optional $ lexeme (MC.string "deriving") *> lexeme (M.some pNameUpper)

pCombo :: Parser Combo
pCombo = Combo <$> pNameUpper <*> pTbl <*> pDev

pFldName :: Parser FldName
pFldName = pNameLower

pFldTitle :: Parser (Maybe FldTitle)
pFldTitle = pString

pFldNameAndTitle :: Parser (FldName, Maybe FldTitle)
pFldNameAndTitle = (,) <$> pFldName <*> pFldTitle

pFldTyp :: Parser FldTypS
pFldTyp =
  try (unwords <$> (((<>) . pure . unwords <$> M.some pNameUpper) <*> (pure . unwords <$> M.some pFldTyp)))
    <|> try (parens' pFldTyp)
    <|> try (brackets' pFldTyp)
    <|> try pTuple
    <|> (unwords <$> M.some pNameUpper)

pFldSamp :: Parser (Maybe FldSamp)
pFldSamp = pString

pFldTypTup :: Parser (FldTypS, MaybeFldBas, Maybe FldSamp)
pFldTypTup = do
  (_fldTypS, _mFldBas) <- lexeme ((,) <$> (pNameUpper <|> parens pFldTyp <|> brackets' pFldTyp) <*> (True <$ MC.char '?' <|> pure False))
  _mFldSamp <- pFldSamp
  pure (_fldTypS, _mFldBas, _mFldSamp)

pTblFld :: Parser (Maybe TblFld)
pTblFld =
  optional $
    try (parens pTblFldR)
      <|> try (parens pTblFldWR)
      <|> try (parens pTblFldNR)
      <|> try (parens pTblFldNWR)
  where
    pFldO = unwords <$> (((<>) . pure <$> (symbol "FieldNullable" <|> symbol "Field")) <*> (pure <$> pNameUpper))
    pFldM = unwords <$> ((<>) . pure <$> symbol "Maybe" <*> (pure <$> parens' pFldO))
    pTblFldR = TblFldR <$> pFldO
    pTblFldWR = TblFldWR <$> (pFldM <|> pFldO) <*> (symbol "," *> pFldO)
    pTblFldNR = TblFldNR <$> quotes pName <*> (symbol "," *> (pFldM <|> pFldO))
    pTblFldNWR = TblFldNWR <$> quotes pName <*> (symbol "," *> (pFldM <|> pFldO)) <*> (symbol "," *> pFldO)

pExtName :: Parser (ExtTypName, MaybeFldExt)
pExtName = lexeme ((,) <$> pNameUpper <*> (True <$ MC.char '?' <|> pure False))

pField :: Parser Field
pField = Field <$> pFldNameAndTitle <*> pFldTypTup <*> pTblFld <*> M.many pExtName

pTt :: Parser [TtRep]
pTt = M.many . MCL.indentBlock scn $ do
  combo <- pCombo
  return $ MCL.IndentSome Nothing (return . TtRep combo) pField

--

devIns :: [String]
devIns = ["Eq", "Show", "Generic"]

noDevIns :: [String]
noDevIns = ["Default", "ToJSON", "FromJSON", "ToSchema"]

ttRep :: TtRep -> (HM.HashMap TypName [(FldName, Maybe FldTitle, FldTypS, MaybeFldBas, Maybe FldSamp, MaybeFldExt)], [DevClsName])
ttRep (TtRep (Combo basTypName _ devs) flds) =
  let hm = defBase basTypName (flds <&> (\(Field tupleNameTitle tup _ _) -> (tupleNameTitle, tup))) HM.empty
   in (defExts flds hm, devIns <> filter (`notElem` (devIns <> noDevIns)) (fromMaybe empty devs))
  where
    defBase _basTypName tuples = HM.insert _basTypName (tuples <&> (\((_fldName, _mFldTitle), (_fldTypS, _maybeFldBas, _mFldSamp)) -> (_fldName, _mFldTitle, _fldTypS, _maybeFldBas, _mFldSamp, False)))
    defExts [] m = m
    defExts (Field _ _ _ [] : ds) m = defExts ds m
    defExts (Field (_fldName, _mFldTitle) (_fldTypS, _mFldBas, _mFldSamp) _mTblFld ((_extTypName, _mFldExt) : exTups) : _flds) m =
      let m' = HM.insert _extTypName (snoc (HM.lookupDefault [] _extTypName m) (_fldName, _mFldTitle, _fldTypS, _mFldBas, _mFldSamp, _mFldExt)) m
       in defExts (Field (_fldName, _mFldTitle) (_fldTypS, _mFldBas, _mFldSamp) _mTblFld exTups : _flds) m'

tbRep :: [Field] -> [(TblFldName, (FldTypS, MaybeFldBas), TblFldTypSW, TblFldTypSR)]
tbRep = mapMaybe tblFld
  where
    tblFld (Field (_fldName, _) (_fldTypS, _mFldBas, _mFldSamp) mTblFld _) = case mTblFld of
      Nothing -> Nothing
      Just _tblFld -> case _tblFld of
        TblFldR _tblFldTypSR -> Just (_fldName, (_fldTypS, _mFldBas), _tblFldTypSR, _tblFldTypSR)
        TblFldWR _tblFldTypSW _tblFldTypSR -> Just (_fldName, (_fldTypS, _mFldBas), _tblFldTypSW, _tblFldTypSR)
        TblFldNR _tblFldName _tblFldTypSR -> Just (_tblFldName, (_fldTypS, _mFldBas), _tblFldTypSR, _tblFldTypSR)
        TblFldNWR _tblFldName _tblFldTypSW _tblFldTypSR -> Just (_tblFldName, (_fldTypS, _mFldBas), _tblFldTypSW, _tblFldTypSR)

-- tedious type
tt :: QuasiQuoter
tt =
  QuasiQuoter
    { quoteExp = error "tt cannot be used as exp",
      quotePat = error "tt cannot be used as pat",
      quoteType = error "tt cannot be used as type",
      quoteDec = ttDec
    }

ttDec ::
  String ->
  Q [Dec]
ttDec str = do
  let tts = case parse pTt "" str of
        Left b -> fail (errorBundlePretty b)
        Right v -> v
  let ttReps = tts <&> ttRep
  let reps = ttReps >>= (\(m, devs) -> HM.toList m <&> (\(n, flds) -> (n, devs, flds)))
  typDecs <- join <$> mapM repDec reps
  tblDecs <- join <$> mapM tbDec tts
  return (typDecs <> tblDecs)
  where
    repDec :: (TypName, [DevClsName], [(FldName, Maybe FldTitle, FldTypS, MaybeFldBas, Maybe FldSamp, MaybeFldExt)]) -> Q [Dec]
    repDec (typName, _devClsNames, flds) = do
      let name = mkName typName
      let vbs =
            flds
              <&> ( \(_fldName, _, _fldTypS, _mFldBas, _mFldSamp, _mFldExt) ->
                      varBangType (mkName $ "_" <> lowerFirst typName <> upperFirst _fldName) (bangType (bang noSourceUnpackedness noSourceStrictness) (pure $ strToTyp _fldTypS (_mFldBas || _mFldExt)))
                  )
      dec <-
        if length vbs == 1
          then
            newtypeD mempty name [] Nothing (recC name vbs) [derivClause Nothing $ _devClsNames <&> conT . mkName]
          else
            dataD mempty name [] Nothing [recC name vbs] [derivClause Nothing $ _devClsNames <&> conT . mkName]
      decDefault <- do
        return $ StandaloneDerivD (Just AnyclassStrategy) [] (AppT (ConT ''Default) (ConT name))
      decToJSON <- do
        eToJ <- [|genericToJSON toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
        let fToJ = FunD 'A.toJSON [Clause [] (NormalB eToJ) []]
        eToE <- [|genericToEncoding toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
        let fToE = FunD 'A.toEncoding [Clause [] (NormalB eToE) []]
        return $ InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT name)) [fToJ, fToE]
      decFromJSON <- do
        e <- [|genericParseJSON toJSONOptions {A.fieldLabelModifier = trimPrefixName_ typName}|]
        return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT name)) [FunD 'A.parseJSON [Clause [] (NormalB e) []]]
      decToSchema' <- do
        let tuples =
              ( \(_fldName, _mFldTitle, _fldTypS, _mFldBas, _mFldSamp, _mFldExt) -> do
                  let _fldTyp = strToTyp _fldTypS (_mFldBas || _mFldExt)
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
                          <&> ( \(_fldName, _mFldTitle, _, _, _, _mFldBas) ->
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
        let pureStmt = noBindS (appE (varE 'return) (appE (appE (varE 'named) (stringE typName)) (uInfixE (uInfixE (uInfixE (uInfixE (varE 'mempty) (varE '(F.&)) u1) (varE '(F.&)) u2) (varE '(F.&)) u3) (varE '(F.&)) u4)))
        e <- doE $ bindStmts <> [pureStmt]
        return $ InstanceD Nothing [] (AppT (ConT ''ToSchema) (ConT name)) [FunD 'O.declareNamedSchema [Clause [WildP] (NormalB e) []]]
      decLens <- dropWhile isDataD <$> declareLensesWith lensRules (pure [dec])
      pure $ [dec, decDefault, decToJSON, decFromJSON, decToSchema'] <> decLens
    isDataD :: Dec -> Bool
    isDataD DataD {} = True
    isDataD NewtypeD {} = True
    isDataD _ = False
    fldSchemaName :: FldName -> String
    fldSchemaName = ("schema" <>) . upperFirst

tbDec :: TtRep -> Q [Dec]
tbDec (TtRep (Combo basTypName tblName _) flds) = evalContT $ do
  callCC $ \exit -> do
    let funbasTypName = lowerFirst basTypName <> "Table"
    let tblFlds = tbRep flds
    when (null tblFlds) $ exit mempty
    let wTyps = (`strToTyp` False) . sel3 <$> tblFlds
    let wFlds = genSigFields wTyps Nothing
    let vTyps = (`strToTyp` False) . sel4 <$> tblFlds
    let vFlds = genSigFields vTyps Nothing
    sig <- lift $ sigD (mkName funbasTypName) (appT (appT (conT ''Table) wFlds) vFlds)
    let nFlds = sel1 <$> tblFlds
    let eFlds = appE (varE 'tableField) . litE . stringL <$> nFlds
    fun <- lift $ funD (mkName funbasTypName) [clause [] (normalB (appE (appE (varE 'table) (litE (stringL (fromMaybe (lowerFirst basTypName) tblName)))) (appE (varE (mkName $ "p" <> (show . length $ nFlds))) (genFunFields eFlds)))) []]
    return [sig, fun]
  where
    genSigFields (t : ts) Nothing = genSigFields ts (Just $ if null ts then t else AppT (TupleT $ length ts + 1) t)
    genSigFields (t : ts) (Just t') = genSigFields ts (Just (AppT t' t))
    genSigFields [] (Just t') = return t'
    genSigFields [] Nothing = return $ ConT ''()
    genFunFields es | length es > 1 = tupE es
    genFunFields [e] = parensE e
    genFunFields _ = fail "makeTable: empty flds"

strToTyp :: String -> Bool -> Type
strToTyp s m =
  let ot = either (error "ttDec: cannot parse field type") id (parseType s)
   in if m then AppT (ConT ''Maybe) ot else ot

isMaybeTyp :: Type -> Bool
isMaybeTyp (AppT (ConT c) _)
  | c == ''Maybe = True
  | otherwise = False
isMaybeTyp _ = False

--

{-
> :set -XTemplateHaskell
> $(stringE . show =<< reify ''Hello)
> parseTest pName "Hello"
-}
