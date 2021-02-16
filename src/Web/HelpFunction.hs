module Web.HelpFunction where

import ClassyPrelude
    ( ($),
      Monad(return),
      Applicative(pure),
      Maybe(..),
      ByteString,
      either,
      (++),
      pack,
      IsMap(lookup),
      LazySequence(fromStrict),
      Utf8(encodeUtf8) )
import Entity.ExportEntity
    ( errorText,
      ErrorServer(ErrorConvert, ErrorGetCookie),
      SessionId(SessionId) )
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import Data.Aeson ( ToJSON(toEncoding), fromEncoding )
import Control.Monad.Except ( MonadError(throwError) )
import Text.Parsec as Parsec
    ( Parsec, char, digit, letter, many1, (<|>), parse )

serverNotAcceessAuthor :: Monad m =>  m HTTP.Response
serverNotAcceessAuthor = do
        pure $ HTTP.responseLBS HTTP.status404 [] "Not Author Access" 

serverNotAcceessAdmin :: Monad m =>  m HTTP.Response
serverNotAcceessAdmin = do
         pure $ HTTP.responseLBS HTTP.status404 [] "Not Admin Access" 

serverErrorResponse :: Monad m => ErrorServer ->  m HTTP.Response
serverErrorResponse err = do
        pure $ HTTP.responseLBS HTTP.status404 [] (encodeUtf8 $ fromStrict $ errorText err)

successResponse :: forall a. ToJSON a  => a  -> HTTP.Response
successResponse  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  b

getCookie :: MonadError ErrorServer m => HTTP.Request -> m SessionId
getCookie  req =
        convertRequestToCookie $ lookup "Cookie" $ HTTP.requestHeaders req

convertRequestToCookie :: MonadError ErrorServer m =>  Maybe ByteString -> m SessionId
convertRequestToCookie Nothing = throwError ErrorGetCookie
convertRequestToCookie (Just byt) = 
                either (\_ -> throwError ErrorConvert) return (Parsec.parse parserCookie "" byt)

parserCookie :: Parsec.Parsec ByteString () SessionId
parserCookie = do
     _ <- Parsec.many1 (Parsec.letter Parsec.<|> Parsec.digit Parsec.<|> Parsec.char ':')
     _ <- Parsec.char '='
     value <- Parsec.many1 (Parsec.letter Parsec.<|> Parsec.digit Parsec.<|> Parsec.char ':') 
     return $ SessionId $ pack value

setCookie :: MonadError ErrorServer m => SessionId  -> m HTTP.Response
setCookie  (SessionId sess)  = return $ HTTP.responseLBS HTTP.status200 [("SetCookie", "sId=" ++ encodeUtf8 sess)] "SET-COOKIE"

