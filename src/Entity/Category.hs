module Entity.Category   where


import ClassyPrelude
    ( ($),
      Eq((==)),
      Show,
      Applicative(pure, (<*>)),
      Generic,
      Int,
      Maybe(..),
      Text,
      (<$>),
      (++),
      unpack )
import qualified Data.Attoparsec.ByteString.Char8 as A
import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import Entity.ImportLibrary
    ( FromJSON,
      ToJSON,
      Value(Null),
      Action(Plain, Many),
      char8,
      field,
      toJSONField,
      FromRow(..),
      ToField(..),
      ToRow(..) )
import Entity.ParseRowEntity
    ( fromPGRow', parseMaybeInt, textContent )
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import qualified Prelude as P

data Category =
  Category
    { idCategory :: Int
    , nameCategory :: Text
    , parentCategory :: Maybe Category
    }
  deriving (Eq, Show, Generic)

instance FromRow Category

instance FromField Category where
  fromField = fromJSONField

instance ToRow Category where
  toRow (Category idC nameC parC) = [toField idC, toField nameC, toField parC]

instance ToField Category where
  toField (Category idC nameC Nothing) =
    Many $
    Plain (char8 '(') :
    toField idC :
    toField nameC :
    toField Null : [Plain (char8 ')')]
  toField (Category idC nameC (Just cat)) =
    Many $
    Plain (char8 '(') :
    toField idC : toField nameC : toField (idCategory cat) : [Plain (char8 ')')]

instance ToJSON Category

instance FromJSON Category

data CategoryRaw =
  CategoryRaw
    { idCategoryRaw :: Int
    , nameCategoryRaw :: Text
    , parentCategoryRaw :: Maybe Int
    }
  deriving (Eq, Show, Generic)

instance FromRow CategoryRaw where
  fromRow = CategoryRaw <$> field <*> field <*> field

instance ToRow CategoryRaw

instance ToField CategoryRaw where
  toField u = toJSONField u

instance FromField CategoryRaw where
  fromField f mb = fromPGRow' parseCategoryRaw f mb

instance ToJSON CategoryRaw

instance FromJSON CategoryRaw

newtype TestArrayCategoryRaw =
  TestArrayCategoryRaw
    { arraysCategoryRaw :: PGArray CategoryRaw
    }
  deriving (Show, Generic)

instance FromJSON (PGArray CategoryRaw)

instance ToJSON (PGArray CategoryRaw)

deriving instance
         Generic (PGArray CategoryRaw) => Generic (PGArray CategoryRaw)

parseCategoryRaw :: A.Parser CategoryRaw
parseCategoryRaw = do
  _ <- A.char '('
  idCat <- textContent
  _ <- A.char ','
  nameCat <- textContent
  _ <- A.char ','
  parentCat <- textContent
  _ <- A.char ')'
  pure
    (CategoryRaw
       (P.read $ ClassyPrelude.unpack idCat)
       nameCat
       (parseMaybeInt parentCat))



                  -- Convert CategoryRaw -> Category


convertCategoryRawArray :: [CategoryRaw] -> [Category]
convertCategoryRawArray rawA = convertToCategoryThirdStep catA rawA2
  where 
    (catA, rawA2) = convertToCategoryFirstStep rawA
  

convertToCategoryThirdStep :: [Category] -> [CategoryRaw] -> [Category]
convertToCategoryThirdStep [] (_:_) = []
convertToCategoryThirdStep catA [] = catA
convertToCategoryThirdStep (x:xs) arrCatRaw = convertToCategoryThirdStep (xs ++ arrCat) arrCatRaw'
  where
    (arrCat,arrCatRaw') =  convertToCategorySecondStep x arrCatRaw 


convertToCategorySecondStep :: Category -> [CategoryRaw] -> ([Category],[CategoryRaw])
convertToCategorySecondStep  cat arr   =  convertToCategorySecondStep'  cat arr ([],[])
  where
    convertToCategorySecondStep' cats [] ([],rawA) = ([cats],rawA)
    convertToCategorySecondStep' _ [] (catA ,rawA) = (catA,rawA)
    convertToCategorySecondStep' cat' (x:xs) (catA, rawA) = case convertCategory cat' x of
      Nothing -> convertToCategorySecondStep' cat' xs (catA, rawA ++ [x])
      Just cat2 -> convertToCategorySecondStep' cat' xs (catA ++ [cat2], rawA)

convertToCategoryFirstStep :: [CategoryRaw]  -> ([Category],[CategoryRaw])
convertToCategoryFirstStep  = convertToCategoryFirstStep' ([],[]) 
  where
    convertToCategoryFirstStep' parArray [] =  parArray
    convertToCategoryFirstStep' (cat,catR) (x:xs) = case convertMainCategory x of
      Just y -> convertToCategoryFirstStep' ( y : cat,catR) xs
      Nothing -> convertToCategoryFirstStep' (cat, x : catR) xs
      

convertMainCategory :: CategoryRaw -> Maybe Category
convertMainCategory (CategoryRaw idC nameC Nothing) = Just (Category idC nameC Nothing)
convertMainCategory (CategoryRaw idC nameC (Just 0)) = Just (Category idC nameC Nothing)
convertMainCategory (CategoryRaw _ _ (Just _)) =  Nothing

convertCategory :: Category -> CategoryRaw -> Maybe Category
convertCategory (Category _ _ _) (CategoryRaw _ _ Nothing) = Nothing
convertCategory (Category idC nameC inCat) (CategoryRaw idC1 nameC1 (Just idP)) = if idC == idP 
  then Just $  Category idC1 nameC1 (Just $ Category idC nameC inCat) 
  else Nothing