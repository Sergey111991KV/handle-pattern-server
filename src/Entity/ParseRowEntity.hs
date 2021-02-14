module Entity.ParseRowEntity where


import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Time 
import Data.Typeable
import Control.Applicative
import Database.PostgreSQL.Simple.FromField
import Data.ByteString
import qualified Data.Text  as T
import qualified Data.Text.Encoding as T


fromPGRow' ::
     Typeable a => A.Parser a -> Field -> Maybe ByteString -> Conversion a
fromPGRow' _ f Nothing = returnError UnexpectedNull f ""
fromPGRow' parser f (Just bs) = do
  case A.parseOnly parser bs of
    Left err -> returnError ConversionFailed f err
    Right a -> pure a

time :: UTCTime
time = read "1970-01-01 00:00:00.000000 UTC" :: UTCTime

timeToByteStr :: UTCTime -> ByteString
timeToByteStr = B.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

timeFromByteString :: T.Text -> UTCTime
timeFromByteString s = maybe time zonedTimeToUTC (timeFromByteString' s)

timeFromByteString' :: T.Text -> Maybe ZonedTime
timeFromByteString' s =
  parseTimeM True defaultTimeLocale "%Y" (unpack s) :: Maybe ZonedTime

fromJust :: [Maybe a] -> [a]
fromJust [] = []
fromJust (Nothing:xs) = fromJust xs
fromJust (Just a:xs) = a : fromJust xs

parseMaybeInt :: T.Text -> Maybe Int
parseMaybeInt txtParse =
  case txtParse of
    "null" -> Nothing
    _ -> Just $ read $ unpack txtParse

textContent :: A.Parser T.Text
textContent = T.decodeUtf8 <$> (quoted <|> plain)

quoted :: A.Parser ByteString
quoted = A.char '"' *> A.option "" contents <* A.char '"'
  where
    esc = A.char '\\' *> (A.char '\\' <|> A.char '"')
    unQ = A.takeWhile1 (A.notInClass "\"\\")
    contents = mconcat <$> many (unQ <|> B.singleton <$> esc)

plain :: A.Parser ByteString
plain = A.takeWhile1 (A.notInClass ",\"()")
