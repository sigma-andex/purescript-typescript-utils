module Typescript.Utils.Enum where

import Prelude

import Data.Function (applyFlipped)
import Data.Function.Uncurried (Fn2, runFn2)
import Type.Data.List (type (:>), List', Nil', class Concat)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class IsMatch :: forall (e :: Type). Type -> (e -> Type) -> e -> Constraint
class
  IsMatch enum enumCtr enumVal
  | enum -> enumCtr enumVal
  , enumCtr enumVal -> enum where
  isMatch :: enum -> enumCtr enumVal -> Boolean

foreign import isMatchImpl :: forall e ctr v. Fn2 e (ctr v) Boolean

instance IsMatch e ctr v where
  isMatch = runFn2 isMatchImpl

-- | Typeclass for indicating the order of the enumeration
class Enum :: forall k1 k2. (k1 -> Type) -> k1 -> k2 -> Constraint
class Enum enumCtr enumCur enumNext | enumCtr enumCur -> enumNext where
  enumValue :: enumCtr enumCur

class EnumRep :: forall k. (k -> Type) -> k -> Constraint
class EnumRep enumCtr enumCur where
  enumRep :: enumCtr enumCur

-- | Typeclass for constructing an enumeration
class EnumConfig :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class EnumConfig enum enumCtr enumVal | enum -> enumCtr enumVal

class MatchHelper :: forall k1 k2. k1 -> k2 -> Type -> Type -> Type -> Constraint
class MatchHelper enumCtr enumVal enum output fun | enumCtr enumVal enum output -> fun where
  matchHelper
    :: Proxy enumCtr
    -> Proxy enumVal
    -> Proxy enum
    -> output
    -> (enum -> (enum -> output) -> output)
    -> fun

instance MatchHelper enumCtr Unit enum output (enum -> output) where
  matchHelper _ _ _ output continuation = flip continuation (const output)
else instance
  ( IsMatch enum enumCtr enumVal
  , Enum enumCtr enumVal nextEnumValue
  , MatchHelper enumCtr nextEnumValue enum output nextResult
  ) =>
  MatchHelper enumCtr enumVal enum output (((enumCtr enumVal -> output) -> nextResult)) where
  matchHelper _ _ _ output continuation =
    let
      handler :: (enumCtr enumVal -> output) -> nextResult
      handler applyMatch =
        let
          nextCont enum otherwise =
            continuation enum
              $ \i ->
                  let
                    matchEnumValue :: enumCtr enumVal
                    matchEnumValue = enumValue
                  in
                    if (isMatch i matchEnumValue) then
                      applyMatch matchEnumValue
                    else
                      otherwise i

          nextResult :: nextResult
          nextResult = matchHelper (Proxy :: Proxy enumCtr) (Proxy :: Proxy nextEnumValue) (Proxy :: Proxy enum) output nextCont
        in
          nextResult
    in
      handler

-- | Typeclass for matching enum values
class Match :: forall k. k -> Type -> Type -> Constraint
class Match enum output result | enum output -> result where
  -- | Creates a matching function for enums 
  -- | Usage:
  -- | ```purescript
  -- | default :: String 
  -- | default = "some-arbitrary-value-of-some-arbitrary-type"
  -- | 
  -- | f = match (Proxy :: Proxy MyEnum) default
  -- | ```
  match :: Proxy enum -> output -> result

instance (EnumConfig enum enumCtr enumVal, MatchHelper enumCtr enumVal enum output fun) => Match enum output fun where
  match :: Proxy enum -> output -> fun
  match enumProxy output = matchHelper (Proxy :: Proxy enumCtr) (Proxy :: Proxy enumVal) enumProxy output applyFlipped

class SplitByElem :: forall k. k -> List' k -> List' k -> List' k -> Constraint
class SplitByElem t tlistin tlistbefore tlistafter | t tlistin -> tlistbefore tlistafter

instance SplitByElem t Nil' other Nil'
else instance SplitByElem t (t :> after) Nil' after
else instance (SplitByElem t ts nextbefore nextafter) => SplitByElem t (other :> ts) (other :> nextbefore) nextafter

class RemoveElem :: forall k. k -> List' k -> List' k -> Constraint
class RemoveElem t tlistin tlistout | t tlistin -> tlistout

instance RemoveElem t Nil' Nil'
else instance
  ( SplitByElem t (x :> xs) before after
  , Concat before after out
  ) =>
  RemoveElem t (x :> xs) out

foreign import isTListMatchImpl :: forall tlist ctr v. Fn2 tlist (ctr v) Boolean

-- TODO: Clean up the match functions
isTListMatch :: forall tlist ctr v. tlist -> ctr v -> Boolean
isTListMatch = runFn2 isTListMatchImpl

default :: forall b tlist. b -> Proxy tlist -> b
default = const

on
  :: forall p t tlist tlistout b
   . RemoveElem t tlist tlistout
  => p t
  -> (p t -> b)
  -> (Proxy tlistout -> b)
  -> Proxy tlist
  -> b
on t f otherwise tlist =
  if isTListMatch tlist t then
    f t
  else
    otherwise (unsafeCoerce tlist)