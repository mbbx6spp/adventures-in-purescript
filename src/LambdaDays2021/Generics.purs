module LambdaDays2021.Generics
       ( Product (..)
       , Sum (..)
       ) where

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep as G
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)

data Sum a b = Inl a | Inr b
data Product a b = Product a b

-- Type operators for Sum and Product
infixl 6 type Sum as :+:
infixl 7 type Product as :*:

-- Operator for the Product data constructor
infixl 7 Product as :*:

type MaybeRep a = Unit :+: a

repFromMaybe :: forall a. Maybe a -> MaybeRep a
repFromMaybe = case _ of
  Nothing -> Inl unit
  Just x -> Inr x

repToMaybe :: forall a. MaybeRep a -> Maybe a
repToMaybe = case _ of
  Inl _ -> Nothing
  Inr x -> Just x
