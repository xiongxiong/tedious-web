# tedious-web

A user-friendly web development tool that can easily define multiple interrelated data types, automatically generate common type class instances, provide some convenience for web development.

## Pros & Cons

### Pros:
* Can easily define multiple interrelated data types.
* Instances of Show, Eq, Generic, Default ([data-default](https://hackage.haskell.org/package/data-default)), ToJSON ([aeson](https://hackage.haskell.org/package/aeson)), FromJSON ([aeson](https://hackage.haskell.org/package/aeson)), ToSchema ([openapi](https://hackage.haskell.org/package/openapi3)) are created by default.
* The table definition of the data type in the style of [opaleye](https://hackage.haskell.org/package/opaleye) will be created easily.
* Database migration statements based on [persistent](https://hackage.haskell.org/package/persistent) can also be easily generated.
* Field types support type variables.

### Cons:
* Do not support sum type.
* Do not support complex requirements.

## Example

```haskell
[tedious|

Person table t_persion deriving Exception -- optional comment of type
  id `ID` Int64 (Maybe (Field SqlInt8), Field SqlInt8) PersonToUpd
  firstName `first name` Text `John` ("first_name", Field SqlText) !UniquePersonName PersonToAdd PersonToUpd?
  lastName `second name` Text `Smith` ("first_name", Field SqlText) !UniquePersonName PersonToAdd PersonToUpd?
  age Int `10` (Field SqlInt4) PersonToAdd -- optional comment of field A
  address `home address` Text? (Maybe (Field SqlText), Field SqlText) PersonToAdd
  profession Text? `teacher` ("profes", Maybe (Field SqlText), Field SqlText) PersonToAdd
  hobby Text? (Maybe (Field SqlText), Field SqlText) PersonToAdd PersonToUpd?
  score `(English, Mathematic)` ((Int, Int))
  other a

|]
```

will generate:

```haskell
data Person
  = Person {  _personId :: Int64,
              _personFirstName :: Text,
              _personLastName :: Text,
              _personAge :: Int,
              _personAddress :: (Maybe Text),
              _personProfession :: (Maybe Text),
              _personHobby :: (Maybe Text),
              _personScore :: (Int, Int)
            }

deriving instance Exception Person
deriving stock instance Show Person
deriving stock instance Eq Person
deriving stock instance Generic Person
deriving instance Default Person
instance ToJSON Person where ...
instance FromJSON Person where ...
instance ToSchema Person where ...

data PersonToAdd
  = PersonToAdd { _personToAddFirstName :: Text,
                  _personToAddLastName :: Text,
                  _personToAddAge :: Int,
                  _personToAddAddress :: (Maybe Text),
                  _personToAddProfession :: (Maybe Text),
                  _personToAddHobby :: (Maybe Text)
                }

deriving instance Exception PersonToAdd
deriving stock instance Show PersonToAdd
deriving stock instance Eq PersonToAdd
deriving stock instance Generic PersonToAdd
deriving instance Default PersonToAdd
instance ToJSON PersonToAdd where ...
instance FromJSON PersonToAdd where ...
instance ToSchema PersonToAdd where ...

data PersonToUpd
  = PersonToUpd { _personToUpdId :: Int64,
                  _personToUpdFirstName :: (Maybe Text),
                  _personToUpdLastName :: (Maybe Text),
                  _personToUpdHobby :: (Maybe Text)
                }

deriving instance Exception PersonToUpd
deriving stock instance Show PersonToUpd
deriving stock instance Eq PersonToUpd
deriving stock instance Generic PersonToUpd
deriving instance Default PersonToUpd
instance ToJSON PersonToUpd where ...
instance FromJSON PersonToUpd where ...
instance ToSchema PersonToUpd where ...

personTable :: Table  ( Maybe (Field SqlInt8), 
                        Field SqlText, 
                        Field SqlText, 
                        Field SqlInt4, 
                        Maybe (Field SqlText), 
                        Maybe (Field SqlText), 
                        Mayb(Field SqlText)
                      ) 
                      ( Field SqlInt8,
                        Field SqlText,
                        Field SqlText,
                        Field SqlInt4,
                        Field SqlText,
                        Field SqlText,
                        Field SqlText
                      )
personTable = ...

tediousPersistString :: String
tediousPersistString = ...
```

You can use tediousPersistString to do migration in another module like this:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(quoteExp persistUpperCase tediousPersistString)
```