{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module AesonArrayGeneric (
  GFromArrayJSON, genericParseArrayJSON,

  AesonArrayOptions (..), defaultAesonArrayOptions,

  GToArrayJSON, genericToArrayJSON,

  -- GFieldSetJSON, genericFieldSetParseJSON,
  ) where

import GHC.Generics
import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
-- import Data.DList (DList)
-- import qualified Data.DList as DList
-- import Data.Set ((\\))
-- import qualified Data.Set as Set
-- import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.Text as T
import Data.Aeson.Types
  (FromJSON (..), ToJSON (..), genericParseJSON, Parser, Options (..), Value (..))
import Data.Aeson (GFromJSON, Zero)
import Data.Vector (Vector)
import qualified Data.Vector as Vector


-- JSON array からレコードを取り出す
class GFromArrayJSON f where
  gFromArrayJSON :: StateT [Value] Parser (f a)

instance GFromArrayJSON U1 where
  gFromArrayJSON  =  return U1

instance (GFromArrayJSON a, GFromArrayJSON b) => GFromArrayJSON (a :*: b) where
  gFromArrayJSON  =  (:*:) <$> gFromArrayJSON <*> gFromArrayJSON

instance GFromArrayJSON a => GFromArrayJSON (M1 i c a) where
  gFromArrayJSON  =  M1 <$> gFromArrayJSON

instance FromJSON a => GFromArrayJSON (K1 i a) where
  gFromArrayJSON  =  do
    vs'  <-  get
    K1 <$> case vs' of
     v:vs  ->  (lift $ parseJSON v)   <* put vs
     []    ->   lift $ parseJSON Null



genericParseArrayJSON :: (Generic a, GFromJSON Zero (Rep a), GFromArrayJSON (Rep a))
                      => AesonArrayOptions -> Options -> Value -> Parser a
genericParseArrayJSON arrOpt opt = d where
  d (Array vs)      =  do (a, s) <- runStateT gFromArrayJSON $ Vector.toList vs
                          guard (allowSpilledArguemnts arrOpt || null s)
                            <|> fail ("Too many arguments! Spilled arguments: " ++ show s)
                          return $ to a
  d v@(Object _)    =  genericParseJSON opt v
  d _               =  empty

data AesonArrayOptions =
  AesonArrayOptions
  { allowSpilledArguemnts  ::  Bool
  }

defaultAesonArrayOptions :: AesonArrayOptions
defaultAesonArrayOptions =
  AesonArrayOptions
  { allowSpilledArguemnts  =  False
  }


class GToArrayJSON f where
  gToArrayJSON :: f a -> Vector Value

instance GToArrayJSON U1 where
  gToArrayJSON U1 = Vector.empty

instance (GToArrayJSON a, GToArrayJSON b) => GToArrayJSON (a :*: b) where
  gToArrayJSON (x :*: y)  =  gToArrayJSON x Vector.++ gToArrayJSON y

instance GToArrayJSON a => GToArrayJSON (M1 i c a) where
  gToArrayJSON (M1 x)  =  gToArrayJSON x

instance ToJSON a => GToArrayJSON (K1 i a) where
  gToArrayJSON (K1 x)  =  Vector.singleton $ toJSON x

genericToArrayJSON :: (Generic a, GToArrayJSON (Rep a))
                   => a -> Value
genericToArrayJSON = Array . gToArrayJSON . from

{-
type FieldName = String
type FieldsW = Writer (DList FieldName)

-- 型情報からフィールド名のリストを取り出す
class GFieldSetJSON f where
  gFieldSet :: FieldsW (f a)

instance GFieldSetJSON U1 where
  gFieldSet = return U1

instance (GFieldSetJSON a, GFieldSetJSON b) => GFieldSetJSON (a :*: b) where
  gFieldSet  =  do
    x <- gFieldSet
    y <- gFieldSet
    return (x :*: y)

instance GFieldSetJSON a => GFieldSetJSON (D1 c a) where
  gFieldSet  =  do
    x <- gFieldSet
    return $ M1 x

instance GFieldSetJSON a => GFieldSetJSON (C1 c a) where
  gFieldSet  =  do
    x  <- gFieldSet
    return $ M1 x

instance (GFieldSetJSON a, Selector s) => GFieldSetJSON (S1 s a) where
  gFieldSet  =  do
    x <- gFieldSet
    saveQueriedField $ M1 x

saveQueriedField :: (GFieldSetJSON a, Selector s)
                 => S1 s a p
                 -> FieldsW (S1 s a p)
saveQueriedField m1  =  do
  tell (pure $ selName m1)
  return m1

instance GFieldSetJSON (K1 i a) where
  gFieldSet  =  return $ K1 undefined

genericFieldSetParseJSON :: (Generic a, GFromJSON Zero (Rep a), GFieldSetJSON (Rep a))
                         => AesonArrayOptions
                         -> Options
                         -> Value
                         -> Parser a
genericFieldSetParseJSON = d  where
  d arrOpts opts v@(Object m)  =  do
    let (px, fs)  =  runWriter gFieldSet
        inv  =  Set.fromList (HashMap.keys m) \\
                Set.fromList (map (T.pack . fieldLabelModifier opts) $ DList.toList fs)
    guard (allowNonExistField arrOpts || Set.null inv)
      <|> fail ("object has illegal field: " ++ show (Set.toList inv))
    j  <-  genericParseJSON opts v
    let _ = from j `asTypeOf` px
    return j
  d _       opts v             =
    genericParseJSON opts v
 -}
