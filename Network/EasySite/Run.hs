


{-# LANGUAGE OverloadedStrings, LambdaCase, NoMonomorphismRestriction #-}

-- let the monomorphismrestriction to improve performance
module EasySite.Run(runSite) where

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status

import Text.XML.HXT.Core hiding (xshowBlob) 
import Data.ByteString.Lazy hiding (empty,map,unpack,putStrLn)
import qualified Data.ByteString.Lazy as B hiding (unpack)
import qualified Data.ByteString.UTF8 as BS

import Data.ByteString.Lazy.UTF8
import Data.Text
import Network.HTTP.Types.Header

-----------------------------------------------
-- import Model
-- import View
-- import Controller
-----------------------------------------------
import EasySite.Model
import EasySite.Views
import EasySite.UrlClass
import EasySite.Cookie

-- accept a list.... 
-- main = updateTheDatabase

runSite::(Configurable a,WebPage a,Adresable a) => Dispatch a -> Int -> IO ()
runSite dispatcher port = do putStrLn $ "Server getting prepared on port "++ show port 
                             conf  <- getConfiguration 
                             case conf of

                               Nothing      -> putStrLn "invalid configuration, sorry, aborting :(" 

                               Just st -> do putStrLn "Configuration loaded succesfully."
                                             let application st request cont = dispatch st request dispatcher  >>= cont
                                             run port $ application st   
                                                
-- (State a -> IO b) -> IO b


dispatch::(Configurable a,WebPage a,Adresable a) => State a -> Request -> Dispatch a -> IO Response
dispatch st req dispatcher = case url_ of
                               Just url -> do (cookie,cookieHeaders) <- processCookie req
                                              result                 <- dispatcher st url cookie 
                                              case result of
                                                Page page           -> return $ responseLBS ok200        cookieHeaders                      (extract page)
                                                Redirect url        -> return $ responseLBS seeOther303  (locationHeader url:cookieHeaders) (mockPage url)
                                      --          _                   -> error "the rest of option are not implemented yet (Run.hs)"
                                                --Left  problem        -> return $ responseLBS (errCode problem) [] (extract problem)

                               Nothing  -> do return $ responseLBS notFound404 [] (extract.PageNotFound$show (path,querry))
  where

    url_ = case path of
            [] -> home
            xs -> fromUrl (fmap unpack xs) querry

    querry = queryString req
    path   = pathInfo req 

    
    -- locationHeader:: UUID -> Header
    locationHeader url =  ("Location", BS.fromString $ "/" ++ render  url) :: Header

            
    extract:: (WebPage a) => a -> ByteString
    extract web =  B.concat. ("<!DOCTYPE html>\n" :) .fmap fromString $ runArr arrow ()   
     where  
        arrow = getDisplay web >>> root [] [this]
                >>> 
                writeDocumentToString [ withShowHaskell no  
                                      , withIndent       yes
                                      , withOutputHTML
                                      ] 

    -- todo, better mock page...
    mockPage url =  B.concat. ("<!DOCTYPE html>\n" :) .fmap fromString $ runArr arrow ()   
     where  
        arrow = root [] [link "" url [txt "Click here for manual redirection"]]
                >>> 
                writeDocumentToString [ withShowHaskell no  
                                      , withIndent       yes
                                      , withOutputHTML
                                      ] 

-- make it use
errCode::ErrorDescription -> Status
errCode = \case
             JustAFuckingError         -> notFound404
             QueryElementUnused     _  -> notFound404
             PageNotFound           _  -> notFound404
             ResourceNotFound          -> notFound404






--registerReq::Request -> IO ()
--registerReq _ = return () --TODO once the db stuff is done...


--registerAccess:: ErrorDescription -> Request -> IO ()
--registerAccess _ _ = return () -- todo once we do the db stuff...
