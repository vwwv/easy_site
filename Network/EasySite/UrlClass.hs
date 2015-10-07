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
           , DataKinds
           , LambdaCase
           , UndecidableInstances
           , OverloadedLists
           , MultiParamTypeClasses
           , PolyKinds
           , OverlappingInstances
           #-}


module EasySite.UrlClass ( Form(..)
                         , UrlType(..)
                         , Get(Get)
                         , Get_(..)
                         , Post_(..)
                         , Field(..)
                         , Post(..)
                         , Button()
                         , FieldForm(..)
                         , Sub(..)
                         , fieldsFor
                         , RendableSource (..)
                         ) where


import Generics.Deriving
import Generics.Deriving.Instances
import Text.XML.HXT.Core hiding(Tree)
import Prelude  hiding (lookup,null)
import Data.Map hiding (map)
import Control.Applicative
import Control.Arrow
import Data.Char
import Network.HTTP.Types
import Data.ByteString.UTF8 hiding (map,null)
import Data.List (intercalate)
import Safe
import GHC.Int
import Data.UUID(UUID)
--data From a = Url 


type Post a    = Get a -- use real post....
data Get a     = Get a 
               | Mock
               deriving (Show,Read,Eq,Ord)-- are you sure it should be readeable?
data Sub a     = Sub a deriving (Show,Read,Eq,Ord)

data Phantom (x :: a) = Phantom
 
type Post_     = Get_  -- TODO, update it....
data Get_      = Get_   deriving (Show,Read,Eq,Ord)


data Field a = Field String
data Button  = Button -- we should not use constructor for this one

class FieldForm a where
  field          :: (ArrowXml arr) => String -> Maybe String -> Maybe String -> Field a ->[ arr b XmlTree] -> arr b XmlTree  
  parseAttribute :: Maybe (Maybe String) -> Maybe a -- It should rather be something like Either ---never mind...

--data Field  :: FieldType -> * where
--  TextAreaField ::  String -> Field TextArea
--  PasswordField ::  String -> Field Password
--  ButtonField   ::            Field Button
--  CheckBoxField ::  String -> Field CheckBox 




class RendableSource a where
  render:: a -> String

-- many instnces... this should produce XmlTree instead of String
instance UrlType a => RendableSource (Get_ -> a) where
   render f = let (url,_) = toUrl $ f Get_
               in map sustitute$intercalate "/" url
       
sustitute '_' = '-'
sustitute x   =  x


--render:: (UrlType a) => a -> String
--render a = let (xs,_) = toUrl a 
--            in intercalate "/" xs -- we forget by now about the argument...will need "toUrlEncode::String->String"

----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------
-- TODO, we should change this
fieldsFor::(Form a, UrlType b) => (Get a -> b) -> ([String] ,Field Button,Fields a)
fieldsFor f = let 
                  foo::(Get x -> y) -> Get x -> Get x
                  foo _  a = a

                  mock     = foo f Mock

                  fields   = getFields mock url 
                  (url, _) = toUrl $ f mock

               in (url,Field "button",fields)

--fieldsFor_ ::(Form a, UrlType b) => String -> (Get a -> b) -> (Field Button,Fields a)
--fieldsFor_ str f = let 
--                        foo::(Get x -> y) -> Get x -> Get x
--                        foo _  a = a

--                        mock     = foo f Mock

--                        fields   = getFields mock url 
--                        (url, _) = first (++[str])$toUrl $ f mock
--                   in (ButtonField url,fields)
-- make it add errors!
class (Generic a,UrlG (Rep a)) => UrlType a where

  toUrl   ::  a -> ([String],Map String (Maybe String))  
  toUrl   = gToUrl.from 

  fromUrl ::  [String] -> Query -> Maybe a
  fromUrl xs query = let tree = fromList [(toString k,fmap toString v)| (k,v) <- query]
                      in to <$> gFromUrl xs tree 

class ( Generic a
      , GetFormD (Rep a)
      
      ) => Form a where

    type Fields a :: *

    getFields :: Get a -> [String] -> Fields a   -- todo, redo as getFields :: Get a -> Fields a 

    descompose :: a -> Map String (Maybe String)
    descompose = error "we don't need this yet..."

    generate   :: Map String (Maybe String) -> Maybe a 
    generate   = fmap to.generateD

-- altenbraker str 25 a)
-- altinbraken str 25 a) 11:00 
class GetFormD a where
    type FieldsD a :: *
    getFieldsD :: Phantom a -> [String] -> FieldsD a
    generateD  :: Map String (Maybe String) -> Maybe (a b) 


class GetFormS  a where
    type FieldsS a :: *
    getFieldsS :: Phantom a -> [String] -> FieldsS a
    generateS  :: Map String (Maybe String) -> Maybe (a b) 


class GetFormC  (a :: * -> * ) where
    type FieldsC a :: *
    getFieldsC :: Phantom a -> [String] -> FieldsC a
    generateC  :: Map String (Maybe String) -> Maybe (a b) 


instance ( Generic a
         , GetFormD (Rep a)
         ) =>  Form a                             where

    type Fields a = FieldsD (Rep a)
    getFields _   = getFieldsD (Phantom::Phantom (Rep a) )

instance (GetFormC f ) => GetFormD (M1 D c f)          where
    type FieldsD (M1 D c f) = FieldsC f
    getFieldsD _ = getFieldsC (Phantom::Phantom f)
    generateD tree = M1 <$> generateC tree

-- one of them is redundant---> eliminate!!!
instance (GetFormS f,Serialize (FieldsS f) () ) => GetFormC (M1 C c f)          where
    
    type FieldsC (M1 C c f) = Serial (FieldsS f) ()
    
    getFieldsC _ str = serial (getFieldsS (Phantom::Phantom f) str) ()   
    
    generateC tree = M1 <$> generateS tree

--instance (Selector c) => GetFormS (M1 S c (K1 R Bool))  where
--    type FieldsS (M1 S c (K1 R Bool)) = Node (Field CheckBox)

--    getFieldsS _  str = Node (CheckBoxField $selName (undefined :: M1 S c (K1 R Bool) ()))

--    generateS tree = let name = selName (undefined :: M1 S c (K1 R String) ())
--                      in case lookup name tree of
--                          Just (Just x) -> Just . M1 $ K1 True 
--                          _             -> Just . M1 $ K1 False
    


instance (Selector c, FieldForm x) => GetFormS (M1 S c (K1 R x))  where
    type FieldsS (M1 S c (K1 R x)) = Node (Field x)

    getFieldsS _  str = Node (Field $selName (undefined :: M1 S c (K1 R Bool) ()))

    generateS tree = let name = selName (undefined :: M1 S c (K1 R String) ())
                      in fmap (M1 . K1) . parseAttribute $ lookup name tree
                          
    


--instance (Selector c) => GetFormS (M1 S c (K1 R String))  where
--    type FieldsS (M1 S c (K1 R String)) = Node (Field TextArea)
    
--    getFieldsS _  str = Node (TextAreaField $selName (undefined :: M1 S c (K1 R String) ()))

--    generateS tree = let name = selName (undefined :: M1 S c (K1 R String) ())
--                      in case lookup name tree of 
--                          Just Nothing  -> Just . M1 $ K1 ""
--                          Just (Just x) -> Just . M1 $ K1 x
--                          Nothing       -> Nothing
--                      -- M1 . K1 <$> lookup name tree


instance (GetFormS x,GetFormS y) => GetFormS (x :*: y) where

    type FieldsS (x :*: y) = Tree (FieldsS x) (FieldsS y) 

    getFieldsS _ str = Tree (getFieldsS (Phantom::Phantom x) str) (getFieldsS (Phantom::Phantom y) str) 
    
    generateS tree =  (:*:) <$> generateS tree <*> generateS tree

--class GetFormG a where
--    generateG :: Map String String -> Maybe (a b) 

{-

M1 D UrlClass2.D1Foo 
   ( M1 C UrlClass2.C1_0Foo
       ( M1 S UrlClass2.S1_0_0Foo (K1 R Int) :*: M1 S UrlClass2.S1_0_1Foo (K1 R Char)
       )
    )

-}

 


--instance TuplaSerialization (FieldList (a,b),FieldList c) where
--  type Serialized (FieldList (a,b),FieldList c) =  Serialized (FieldList a, Serialized (FieldList b, FieldList c))
class Serialize a b where
  type Serial a b :: *
  serial :: a -> b -> Serial a b 

data Node a   = Node a 
data Tree a b = Tree a b

instance Serialize (Node a) acc where
   type Serial (Node a) acc = (a,acc)
   serial (Node a) acc = (a,acc)

instance (Serialize a (Serial b acc), Serialize b acc) => Serialize (Tree a b) acc where
   type Serial (Tree a b) acc = Serial a (Serial b acc)
   serial (Tree a b) acc = serial a (serial b acc)


--data Get_ :: a -> * where
--   With   :: (Form a) => a -> Get_ (Just a) 
--   End    ::                  Get_ Nothing
-- combine both in one adding an extra argument
-------------------------------------------------------------------------------------------



instance (Generic a,UrlG (Rep a)) => UrlType a 

-------------------------------------------------------------------------------------------
class UrlG a where 
    gToUrl    :: a b                           -> ([String],Map String (Maybe String))
    gFromUrl  :: [String] -> Map String (Maybe String) -> Maybe (a b)

instance (UrlG u,Datatype t) => UrlG (M1 D t u) where
   gToUrl   (M1 u) = gToUrl u
   gFromUrl  a b   = fmap M1 $ gFromUrl a b

instance (UrlG u,Constructor t)    => UrlG (M1 C t u) where 
    
    gToUrl x@(M1 y)      = first ((map (sustitute.toLower) (conName x)) :) $ gToUrl y
    gFromUrl (x:xs) tree = let name = (fmap (sustitute.toLower).conName) (undefined::M1 C t u ())
                          
                           in if x == name 
                               then M1<$>gFromUrl xs tree
                               else Nothing 
    gFromUrl [] _        = Nothing

instance (UrlG u) => UrlG (M1 S x u) where
--instance (UrlG u) => UrlG (M1 S NoSelector u) where
    gToUrl (M1 a) = gToUrl a 
    gFromUrl a  b = fmap M1 $ gFromUrl a b

instance Form a => UrlG (K1 i  (Get a) )   where
    gToUrl   (K1  (Get a)) = ([], descompose a)
    gToUrl   (K1  Mock   ) = ([], fromList  [])

    gFromUrl  xs tree         = fmap (K1 . Get) $ generate tree

instance (Generic a,UrlG (Rep a)) => UrlG (K1 i  (Sub a) ) where
    gToUrl   (K1  (Sub a)) = gToUrl $ from a 
    
    gFromUrl  xs tree      = fmap (K1 . Sub . to) $ gFromUrl  xs tree
    

instance UrlG (K1 i  Get_)   where
    gToUrl _           = ([], fromList [])
    gFromUrl [] tree
       | null tree     = Just $ K1 Get_
       | otherwise     = Nothing
    gFromUrl _  _      = Nothing

instance (UrlG  a, UrlG  b ) => UrlG  (a :+: b) where

    gToUrl  (L1 a)     = gToUrl a
    gToUrl  (R1 b)     = gToUrl b
    gFromUrl  xs  tree = L1 <$> gFromUrl xs tree <|> R1 <$> gFromUrl xs tree          



instance (UrlGI a, UrlG  b ) => UrlG  (a :*: b) where

    gToUrl  (a :*: b)  = first (giToUrl a  ++) $ gToUrl b

    gFromUrl  xs tree  = do (a,xs')  <- giFromUrl xs
                            b        <- gFromUrl  xs' tree
                            return (a :*: b)     
 
-----------------------------------------------------------------------------------------------------

class UrlGI a where
  giToUrl    :: a b      -> [String]
  giFromUrl  :: [String] -> Maybe (a b,[String])

instance (UrlGI u,Datatype t) => UrlGI (M1 D t u) where
   giToUrl   (M1 u) = giToUrl u
   giFromUrl  a     = fmap (first M1) $ giFromUrl a  


instance (UrlGI u,Constructor t)    => UrlGI (M1 C t u) where 
    
    giToUrl x@(M1 y) = map toLower(conName x) : giToUrl y
    giFromUrl (x:xs) = let name = (fmap (sustitute.toLower).conName) (undefined::M1 C t u ())
                          
                       in if x == name 
                           then first M1<$>giFromUrl xs
                           else Nothing 

    giFromUrl []     = Nothing

instance (UrlGI u) => UrlGI (M1 S x u) where
--instance (UrlGI u) => UrlGI (M1 S NoSelector u) where
    giToUrl (M1 a) = giToUrl a 
    giFromUrl      = fmap (first M1).giFromUrl

instance UrlGI U1 where
    giToUrl   _  = []
    giFromUrl xs = Just (U1,xs)
---- undefined, check if it do escape chracter!!! thats quiet important!

instance UrlGI (K1 i UUID) where
   giToUrl   (K1  a) = [show a]
   giFromUrl strs    = case strs of 
                        x:xs 
                         | Just n <- readMay x -> Just (K1 n, xs)
                        _                      -> Nothing
--instance UrlGI (K1 i (KeyBackend backend entity)) where
--   giToUrl   (K1  (Key (PersistInt64 x))) = giToUrl (K1$from64 x) 
--   giFromUrl                              = fmap (first $ K1 . Key . PersistInt64 .to64.unK1) . giFromUrl  

to64   = fromIntegral  :: Int -> GHC.Int.Int64
from64 = fromIntegral :: GHC.Int.Int64 -> Int

instance UrlGI (K1 i  Int)   where
   giToUrl   (K1  a) = [show a]
   giFromUrl strs    = case strs of 
                        x:xs 
                         | Just n <- readMay x -> Just (K1 n, xs)
                        _                      -> Nothing

instance UrlGI (K1 i  String )   where
   giToUrl   (K1  a) = [a]
   giFromUrl strs    = case strs of 
                        x:xs -> Just (K1 x, xs)
                        _    -> Nothing 

instance (Generic c,UrlGI (Rep c)) => UrlGI (K1 i  c )   where
    giToUrl   (K1  a) = giToUrl$from a
    giFromUrl         = fmap (first (K1 . to) ) . giFromUrl


instance (UrlGI a,UrlGI b) => UrlGI (a :+: b) where
        
        giToUrl  (L1 a)    = giToUrl a
        giToUrl  (R1 b)    = giToUrl b
        giFromUrl  xs      = first L1 <$> giFromUrl  xs <|>  first R1 <$> giFromUrl  xs          


instance (UrlGI a,UrlGI b) => UrlGI (a :*: b) where
        
        giToUrl  (a :*: b)  = giToUrl a  ++ giToUrl b
        giFromUrl  xs      = do (a,xs')  <- giFromUrl xs
                                (b,xs'') <- giFromUrl xs'
                                return (a :*: b, xs'')     

-------------------------------------------------------------------------------------------

data Foo=Foo { field1 :: String
             , field2 :: String 
             } deriving (Generic,Show)
