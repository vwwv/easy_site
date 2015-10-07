
{-# LANGUAGE GADTs
           , TypeFamilies
           , FlexibleContexts
           , DefaultSignatures
           , DeriveGeneric
           , NoMonomorphismRestriction
           , TypeSynonymInstances 
           , FlexibleInstances
           , TypeOperators
           , ScopedTypeVariables
           , DataKinds
           , LambdaCase
           , GeneralizedNewtypeDeriving
           #-}

module EasySite.Views where

import Text.XML.HXT.Core
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Data.Text hiding (intercalate,replicate,foldr)
import Data.List 
import Data.Time
import qualified Data.Text as T 
import Generics.Deriving
import Control.Category (Category)
import Safe
import Data.Maybe
import Control.Monad(join)
import EasySite.Model
import EasySite.UrlClass

--------------------------------------------------------------------------------------------------------------------
-- TO implement...once implemented take all the id yooumight find...
-- this could be a lense...
markId::(ArrowXml a)   =>String -> a b XmlTree -> a b XmlTree
markId = addAttribute "id"

addAttribute::(ArrowXml a)   =>String -> String -> a b XmlTree -> a b XmlTree
addAttribute kind value = (>>>addAttr kind value) 
--addAttr
--------------------------------------------------------------------------------------------------------------------
-- auxiliar hxt functions:

 

-- generate a hidden input based on a KeyBackend value...
--hiddenKeyAttribute ::String -> WebArr a b XmlTree
--hiddenKeyAttribute str  = hasName "KeyBackend" /> getText >>>  arr (maybeToList.(readMay::String -> Maybe (KeyBackend foo woo) ))
--                           >>> unlistA >>> arr show >>> mkelem "input" [ sattr "type" "hidden"
--                                                                       , mkText >>> attr "value" this
--                                                                       , sattr "name" str
--                                                                       ] []

checkbox::(ArrowXml a)   =>  String -> a b XmlTree
checkbox str = mkelem "input" [ sattr "type" "checkbox"
                              , sattr "name" str
                              ] []

division::(ArrowXml a) => String  -> [a b XmlTree] -> a b XmlTree
division clss = mkelem "div" [sattr "class" clss]

navbar::(ArrowXml a) => String  -> [a b XmlTree] -> a b XmlTree
navbar clss = mkelem "nav" [sattr "class" clss]

html::(ArrowXml a)   =>  [a b XmlTree] -> a b XmlTree
html = mkelem "html" []

html_head::(ArrowXml a)   =>  [a b XmlTree] -> a b XmlTree
html_head = mkelem "head" [] 

p::(ArrowXml a) => String -> [a b XmlTree] -> a b XmlTree 
p  str     = mkelem "p" [sattr "class" str]

strong_::(ArrowXml a) => String -> a b XmlTree
strong_ str = mkelem "strong" [] [txt str]

codeDef::(ArrowXml a) => String -> a b XmlTree
codeDef        = mkelem "code" [sattr "class" "def"] . return . txt


-- TODO, rebuild this into something more coherent..
htmlElement::(ArrowXml a)=>String -> String -> [a b XmlTree] -> a b XmlTree
htmlElement name clazz = mkelem name [sattr "class" clazz]

spanned::(ArrowXml a) => String -> [a b XmlTree] -> a b XmlTree
spanned = htmlElement "span" 

italic    =  htmlElement "i"
footer    =  htmlElement "footer"
header    =  htmlElement "header"
section   =  htmlElement "section"
h1        =  htmlElement "h1"
pre       =  htmlElement "pre"
img       =  htmlElement "img"
h2        =  htmlElement "h2"
h4        =  htmlElement "h4"
hr        =  htmlElement "hr"
strong    =  htmlElement "strong"
fieldset  =  htmlElement "fieldset"
br        =  htmlElement "br"
label     =  htmlElement "label"
html_body =  htmlElement "body"
ul        =  htmlElement "ul"
nav       =  htmlElement "nav"
tr        =  htmlElement "tr"
td        =  htmlElement "td"
th        =  htmlElement "th"
table     =  htmlElement "table"  
hr_     = hr "" []

-------------------------------------
ul_ clazz = ul clazz. fmap (li "")

attributes::(ArrowXml a) => [(String,String)] -> a b XmlTree -> a b XmlTree
attributes = flip . foldr $ uncurry addAttribute

table__::(ArrowXml a) => String -> [[a b XmlTree]] -> [[[a b XmlTree]]] -> a b XmlTree
table__ clazz headers body = mkelem "table" [sattr "class" clazz]
                                [ mkelem "thead" [] 
                                   [ mkelem "tr" []
                                       [ mkelem "th" [] header
                                       | header <- headers
                                       ]
                                   ]

                                , mkelem "tbody" []
                                   [ mkelem "tr"   []
                                      [ mkelem "td"  [] element
                                      | element <- row
                                      ]
                                   | row <- body
                                   ]
                                ]


p_::(ArrowXml a) => String -> [String] -> a b XmlTree
p_    str     = mkelem "p" [sattr "class" str] . return . txt .  intercalate "\n "

h1_ :: (ArrowXml a) => String -> a b XmlTree
h1_    = mkelem "h1" [] . return . txt

img_ :: (ArrowXml a) => String -> String ->[ a b XmlTree]->  a b XmlTree
img_ clazz ref = mkelem "img" [sattr "class" clazz, sattr "src" ref]

h2_ :: (ArrowXml a) => String -> a b XmlTree
h2_    = mkelem "h2" [] . return . txt

italic_ :: (ArrowXml a) => String -> a b XmlTree
italic_ = mkelem "italic" [] . return . txt

h3 :: (ArrowXml a) => String -> [a b XmlTree] -> a b XmlTree 
h3  clazz = mkelem "h3" [sattr "class" clazz]  

h4_ :: (ArrowXml a) => String -> a b XmlTree
h4_  = mkelem "h4" [] . return . txt


h5 :: (ArrowXml a) => String -> a b XmlTree
h5  = mkelem "h5" [] . return . txt

h5_ :: (ArrowXml a) => String -> [a b XmlTree] -> a b XmlTree
h5_  str = mkelem "h5" [sattr "class" str] 



--ul  :: (ArrowXml a) => String           -> [[a b XmlTree]] -> a b XmlTree
--ul clss = mkelem "ul" [sattr "class" clss] . fmap (mkelem "li" [])

-- TODO, make all the elements with html names, just the html equivalent --> simple and systematic
-- split in, html_equivalent, auxiliar, instances......eliminate all that stuff about xml documents, cool Idea but just that...


list clazz = ul clazz . fmap (mkelem "li" [])

li  :: (ArrowXml a) => String -> [a b XmlTree] -> a b XmlTree
li clazz = mkelem "li" [sattr "class" clazz]

--ul_ :: (ArrowXml a) => String -> String -> [[a b XmlTree]] -> a b XmlTree
--ul_ clss ident = mkelem "ul" [sattr "class" clss,sattr "id" ident] . fmap (mkelem "li" [])

list_ :: (ArrowXml a) => String -> [(String,[a b XmlTree])] -> a b XmlTree
list_ clazz = mkelem "ul" [sattr "class" clazz] . fmap (\ (clazz,content)-> mkelem "li" [sattr "class" clazz] content)

--text_field :: ArrowXml a => String -> String -> a b XmlTree
--text_field str aar = mkelem "input" [sattr "type" "text", sattr "name" str, sattr "value" aar ] []

--text_area ::  ArrowXml a => String -> a b XmlTree
--text_area str = mkelem "textarea" [sattr "name" str] []

--password_field ::  ArrowXml a => a b XmlTree
--password_field = mkelem "input" [ sattr "type" "password"
--                                , sattr "name" "pass"
--                                , sattr "placeholder" "Password"
--                                , sattr "class" "form-control"
--                                ] []

-- <input type="password" name="pass" placeholder="Password" class="form-control" required>
-- <form action="/packages/search" class="search" method="get">
--     <button type="submit">Search</button>&nbsp;<input type="text" name="terms" />
-- </form>

--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
-- maybe we should take the button out of fields
-- fix all this problems!!!! -> do it differently...
-- make button to not be a field mon deux!!


form::(Form a, UrlType b, ArrowXml arr) => String -> (Get a -> b) -> ((Field Button,Fields a) -> [arr c XmlTree] ) -> arr c XmlTree
form clazz action content = let (url,x, xs) = fieldsFor action 
                             in mkelem "form" [ sattr  "action" $ render url 
                                              , sattr  "method" $ "get" -- this should be deduced from the fields...a propos, we should change how
                                              , sattr  "class"  $ clazz -- "form-horizontal" 
                                              , sattr  "role"   $ "form"
                                              ] $ content (x,xs)
 where
    render = ("/"++).intercalate ("/"::String)

-- TODO make field and field_ the same once we take button out!!! important!!
-- TODO, the button stuff!!
-- the label!!!!!!!!

label_::(ArrowXml arr) => String -> String -> Field a -> arr b XmlTree
label_ clazz str f = let 
                        --name = case f of
                        --         TextAreaField x -> x
                        --         PasswordField x -> x
                        --         CheckBoxField x -> x


                     in mkelem "label" [sattr "class" clazz] [txt str]

{-

class FieldForm a where
  field          :: (ArrowXml arr) => String -> Maybe String -> Maybe String -> Field a -> arr b XmlTree 
  parseAttribute :: Maybe (Maybe String) -> Maybe a -- It should rather be something like Either ---never mind...

-}

field_ a b c d = field a b c d []

instance FieldForm String where
  field clazz placeholder value (Field name) = mkelem "input"  ( join [ [sattr "type"  "text"]
                                                                      , [sattr "name"  name  ]
                                                                      , [sattr "class" clazz ]
                                                                      , maybe [] (return.sattr "placeholder") placeholder 
                                                                      , maybe [] (return.sattr "value")       value
                                                                      ]
                                                               )
  parseAttribute (Just Nothing ) = Just ""
  parseAttribute (Just (Just x)) = Just x 
  parseAttribute _               = Nothing

instance FieldForm Button where
  parseAttribute                               = error "EasySite.View.249" --const Nothing
  field clazz placeholder value (Field _) inns = mkelem "button" [sattr "class" clazz,sattr "type" "submit"] 
                                                                   inns
                                                 




--field::(ArrowXml arr) => String -> String -> Field a -> arr b XmlTree  
--field clazz str f = case f of
               
--               TextAreaField name -> mkelem "input" [ sattr "type"  "text"
--                                                    , sattr "name"  name
--                                                    , sattr "placeholder" str
--                                                    , sattr "size"  "34" -- move this to view...
--                                                    , sattr "class" clazz
--                                                    ] []
               
--               PasswordField name -> mkelem "input" [ sattr "type" "password" 
--                                                    , sattr "name"        name
--                                                    , sattr "class"       clazz
--                                                    , sattr "placeholder" str
--                                                    ] []

 

--               CheckBoxField name -> mkelem "input" [ sattr "type"  "checkbox"
--                                                    , sattr "name"  name 
--                                                    , sattr "value" name 
--                                                    , sattr "class"  $ clazz
--                                                    ] []

--field_::(ArrowXml arr) => String -> Field a -> [arr b XmlTree] -> arr b XmlTree
--field_ clazz f = case f of 
--                      ButtonField        -> mkelem "button" [ sattr "type" "submit", sattr "class" clazz] 
                                                     
               
-- link::(RendableSource a) => a -> String -> WebArr c b XmlTree
-- link l name = mkelem "a" [sattr "href" ("/"++render l)] [txt name]
link::(RendableSource a) => String -> a -> [WebArr c b XmlTree] -> WebArr c b XmlTree
link clazz x = a clazz (Just$href x)

a :: String -> Maybe Href -> [WebArr c b XmlTree] -> WebArr c b XmlTree
a clazz Nothing         = mkelem "a" [sattr "class" clazz]
a clazz (Just (Href h)) = mkelem "a" [sattr "class" clazz,sattr "href" h]

a_ clazz (Href h)       = mkelem "a" [sattr "class" clazz,sattr "href" h]

href::(RendableSource a) => a -> Href
href l = Href $ "/"++render  l

button_link::(RendableSource a) => a -> String -> WebArr c b XmlTree
button_link l name =  mkelem "form" [ sattr  "action" $ ("/"++render l)
                                    , sattr  "method" $ "get"
                                    ]
                                    [ mkelem "button" [sattr "type" "submit"] 
                                       [txt name]
                                    ]
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- todo, make class newtyped with isString....
data InputType = Text 
newtype Href   = Href String 

--renderType = \case 
--               Text -> "text"

data Foo = Noo String deriving (Generic)



instance (XmlDocument a) =>XmlDocument [a] where
    rawDocumentXml = rawDocumentListXml

instance (XmlDocument a, XmlDocument b) => XmlDocument (a,b) where
    rawDocumentXml = mkelem "Pair" [] [(arr fst >>> rawDocumentXml) <+> (arr snd >>> rawDocumentXml)]

instance XmlDocument Char where
    rawDocumentXml     = mkelem "Char" [] [arr show >>> mkText] -- generate special characters instead!
    rawDocumentListXml = mkelem "Text" [] [mkText]
    
instance XmlDocument Bool where
    rawDocumentXml = mkelem "Bool" [] [arr show >>> mkText]


instance XmlDocument Double where
    rawDocumentXml = mkelem "Double" [] [arr show >>> mkText] 


instance XmlDocument Int where
    rawDocumentXml = mkelem "Int" [] [arr show >>> mkText] 

-- I think all of this should  go out...
instance (XmlDocument a) => XmlDocument (Maybe a) where
  rawDocumentXml =  arr ( \case
                            Nothing -> ( mkelem "Nothing" [] []              
                                       , ()   
                                       )
                            Just x  -> ( mkelem "Just"    [] 
                                            [ constA x >>> rawDocumentXml
                                            ]
                                       , () 
                                       )
                        ) >>> app 

--instance XmlDocument (KeyBackend a b) where

--   rawDocumentXml     = mkelem "KeyBackend" [] [arr show >>> mkText]






instance                                    GXmlDocument U1 where 
  gRawDocumentXml = none

instance (XmlDocument c) => GXmlDocument (K1 i c) where
  gRawDocumentXml = arr unK1 >>> rawDocumentXml  

instance (GXmlDocument u,Datatype t)  => GXmlDocument (M1 D t u) where 
  gRawDocumentXml = arr unM1 >>> gRawDocumentXml

instance (GXmlDocument u,Constructor t)    => GXmlDocument (M1 C t u) where 
  --gRawDocumentXml = mkelem (datatypeName(undefined :: M1 C t u ())) [] --avoid  undefined usando App...
  --                    [arr unM1 >>> gRawDocumentXml]
  gRawDocumentXml = mkelem (conName(undefined :: M1 C t u ())) [] --avoid  undefined usando App...
                      [arr unM1 >>> gRawDocumentXml]

instance (GXmlDocument u,Selector t)    => GXmlDocument (M1 S t u) where 
  gRawDocumentXml = let name = selName ( undefined :: M1 C t u ())
                     in if name == ""
                         then arr unM1 >>> gRawDocumentXml
                         else mkelem (name) [] 
                                  [arr unM1 >>> gRawDocumentXml]

 

instance (GXmlDocument f,GXmlDocument g) => GXmlDocument (f :+: g)  where
  gRawDocumentXml = (arrL f1>>>gRawDocumentXml) <+> (arrL f2>>>gRawDocumentXml)
    where
        f1 (L1 x) = [x]
        f1 _      = []

        f2 (R1 x) = [x]
        f2 _      = []

instance (GXmlDocument f,GXmlDocument g) => GXmlDocument (f :*: g)  where
  gRawDocumentXml = arr (\(a:*:b) -> (a,b))  
                  >>> ( (arr fst >>> gRawDocumentXml) 
                      <+>  
                        (arr snd >>> gRawDocumentXml) 
                      )
  
--------------------------------------------------------------------------------------------------------------
instance WebPage ErrorDescription where
  getDisplay x = constA x >>> rawDocumentXml

instance XmlDocument ErrorDescription 

--instance XmlDocument () where
--    func = 

---- genericRaDocument::(Generic a, XmlDocument (Rep a x), Arrow arr) => arr a (XmlTree)
--gRawDocumentXml :: forall arr a x . (Generic a, XmlDocument (Rep a x), Arrow arr) => arr a [XmlTree]
--gRawDocumentXml = arr from_ >>> rawDocumentXml
--     where
--        from_ :: a -> Rep a x
--        from_ = from 






















