
{-# LANGUAGE GADTs
           , TypeFamilies
           , FlexibleContexts
           , DefaultSignatures
           , DeriveGeneric
           , StandaloneDeriving
           , TypeSynonymInstances 
           , FlexibleInstances
           , TypeOperators
           , ScopedTypeVariables
           , LambdaCase
           , UndecidableInstances
           , GeneralizedNewtypeDeriving
           #-}

module EasySite.Model where

import Text.XML.HXT.Core
import Network.Wai.Handler.Warp
import Data.Text hiding (intercalate)
import Data.Time
import Control.Category (Category)
--import Data.List(intercalate)
import Generics.Deriving

--import  Control.Applicative
import EasySite.UrlClass
import EasySite.Cookie

{-


-}
-- we do not use this...
data Bind where
  Bind          :: (WebResource a)  =>  Dispatch a -> Port -> Bind


data WebResult a = Page a 
                 | Redirect (Get_ -> Url a)

type Dispatch a = State a ->  Url a -> Cookie -> IO (WebResult a) 



-- this class is actually not needed....
class (Configurable a,WebPage a,Adresable a) =>  WebResource a where
   getConfiguration :: IO (Maybe (State a)) --change this....



instance (Configurable a,WebPage a,Adresable a) => WebResource a where
     getConfiguration = getInitialState
                        

class (UrlType (Url a)) => Adresable a where
  data Url      a   :: *
  home              :: Maybe (Url a)

class Configurable a where
  data State    a   :: *
  
  getInitialState   :: IO (Maybe (State a))

class WebPage a where
  getDisplay :: a -> WebArr a b XmlTree



class NamedModulo a where
  modulo::Location a 
  

instance (GNameModulo (Rep a)) => NamedModulo a where
  modulo = Location$ pathTo (gModulo::Location (Rep a () ))


class GNameModulo a where
  gModulo::Location (a b)

newtype Location a = Location{pathTo::String}

instance (Datatype t) => GNameModulo (M1 D t u) where
  gModulo = Location $ moduloName ( undefined :: M1 D t u x)

 
-- data Displayer a = Displayer (LA XmlTree XmlTree) 
-- we could add a phantom type...just saying...
data ErrorDescription = JustAFuckingError 
                      | QueryElementUnused   String 
                      | PageNotFound         String
                      | ResourceNotFound
                      deriving (Show,Eq,Read,Ord,Generic)


--Use better names....  
--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
newtype WebArr k a b = WebArr (LA a b) deriving ( Arrow
                                                , ArrowXml
                                                , ArrowList
                                                , ArrowApply
                                                , Category
                                                , ArrowPlus
                                                , ArrowZero 
                                                , ArrowTree 
                                                , ArrowIf
                                                ) 
runArr (WebArr x) = runLA x


------------------------------------------------------------------------------------------------------------
-- DEBUGGGG!!!!
class XmlDocument a where 
  
  rawDocumentXml         :: (ArrowList arr,ArrowXml arr)                                 => arr a XmlTree

  default rawDocumentXml :: (Generic a, ArrowList arr,ArrowXml arr,GXmlDocument (Rep a)) => arr a XmlTree
  rawDocumentXml = arr from >>> gRawDocumentXml 

  rawDocumentListXml     :: (ArrowList arr,ArrowXml arr)                                 => arr [a] XmlTree
  rawDocumentListXml = mkelem "List" [] [unlistA >>> rawDocumentXml]
                      -- mkelem "List" [] [unlistA] >>> processChildren rawDocumentXml
class GXmlDocument a where 
  gRawDocumentXml         :: (ArrowList arr,ArrowXml arr) => arr (a x) XmlTree  



-----------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------
moduloName::(Datatype t) => M1 D t u x -> String
moduloName = moduleName

