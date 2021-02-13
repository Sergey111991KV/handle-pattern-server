module Config.ParseConfig where

import qualified Data.Text                as T
import  Text.Parsec 

type ConfigPair = (String, String)

toPairs :: Parsec T.Text () ConfigPair
toPairs = do
     key <- many1 (letter <|> digit <|> char ':')
     spaces
     value <- many1 (letter <|> digit <|> char ':') <|> helpText
     return (key,value)

myParser :: Parsec T.Text () [ConfigPair]
myParser = sepBy toPairs mySeparator

mySeparator :: Parsec T.Text () ()
mySeparator = do
    _ <- char '\n'
    return ()

helpText :: Parsec T.Text () String
helpText = do
     _ <-   char '"'
     value <- many1  (letter <|> digit <|> space  <|> oneOf "'=")
     _ <-   char '"'
     return value 