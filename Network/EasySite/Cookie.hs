
{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralizedNewtypeDeriving #-}



module EasySite.Cookie(processCookie,Cookie) where



import Network.Wai
import Network.HTTP.Types.Header

import Data.ByteString.UTF8(toString,fromString)
import Safe

import Control.Applicative
import Data.Char
import Data.List.Split (wordsBy)
import Database.PostgreSQL.Simple.ToField
import Data.UUID(UUID)
import Data.UUID.V4(nextRandom) 


cookieName :: String
cookieName = "uid"



newtype Cookie = Cookie UUID deriving (Ord,Eq,Read,Show,ToField)


processCookie :: Request -> IO (Cookie,[Header])
processCookie req = case parseCookie req of 
                      
                      Nothing  -> do new <- nextRandom
                                     return (Cookie new,[cookieHeader new])
                      
                      Just old -> return (old,[])


parseCookie :: Request  -> Maybe Cookie 
parseCookie req = foldr (<|>) Nothing [ Cookie <$> actualParse content
                                      | (header,contents) <- requestHeaders req
                                      , "Cookie" == header -- this comparasion is case insensitive
                                      , content <- wordsBy (`elem`"; ")$toString contents
                                      ] 
  
actualParse :: String -> Maybe UUID
actualParse content = case break ('='==) content of
                        (name, '=':value)
                            | strip name == cookieName -> readMay value
                        _                              -> Nothing
 where 
   strip = filter (not.isSpace)




cookieHeader:: UUID -> Header
cookieHeader x =  ("Set-Cookie", fromString$ cookieName++"="++show x)





