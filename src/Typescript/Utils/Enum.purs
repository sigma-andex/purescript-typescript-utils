module Typescript.Utils.Enum where

import Prelude
import Data.Function (applyFlipped)
import Type.Proxy (Proxy(..))

class IsMatch :: forall (e :: Type). Type -> (e -> Type) -> e -> Constraint
class IsMatch enum enumCtr enumValue | enum -> enumCtr enumValue, enumCtr enumValue -> enum where
  isMatch :: enum -> enumCtr enumValue -> Boolean

class Enum :: forall k1 k2. (k1 -> Type) -> k1 -> k2 -> Constraint
class Enum enumCtr enumCur enumNext | enumCtr enumCur -> enumNext where
  enumValue :: enumCtr enumCur

class EnumConfig :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class EnumConfig enum enumCtr enumValue | enum -> enumCtr enumValue

class MatchHelper :: forall k1 k2. k1 -> k2 -> Type -> Type -> Type -> Constraint
class MatchHelper enumCtr enumValue enum output fun | enumCtr enumValue enum output -> fun where
  matchHelper :: Proxy enumCtr -> Proxy enumValue -> Proxy enum -> output -> (enum -> (enum -> output) -> output) -> fun

instance MatchHelper enumCtr Unit enum output (enum -> output) where
  matchHelper _ _ _ output cont = flip cont (const output)
else instance (IsMatch enum enumCtr enumValue, Enum enumCtr enumValue nextEnumValue, MatchHelper enumCtr nextEnumValue enum output nextResult) => MatchHelper enumCtr enumValue enum output (((enumCtr enumValue -> output) -> nextResult)) where
  matchHelper _ _ _ output cont =
    let
      handler :: (enumCtr enumValue -> output) -> nextResult
      handler applyMatch =
        let
          nextCont enum otherwise =
            cont enum
              $ \i ->
                  let
                    matchEnumValue :: enumCtr enumValue
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

class Match :: forall k. k -> Type -> Type -> Constraint
class Match enum output result | enum output -> result where
  match :: Proxy enum -> output -> result

instance (EnumConfig enum enumCtr enumValue, MatchHelper enumCtr enumValue enum output fun) => Match enum output fun where
  match :: Proxy enum -> output -> fun
  match inputProxy output = matchHelper (Proxy :: Proxy enumCtr) (Proxy :: Proxy enumValue) inputProxy output applyFlipped
