module Typescript.Utils.Enum where

import Prelude
import Data.Function (applyFlipped)
import Type.Proxy (Proxy(..))

class IsMatch :: forall (e :: Type). Type -> (e -> Type) -> e -> Constraint
class IsMatch enum enumCtr enumValue | enum -> enumCtr enumValue
, enumCtr enumValue -> enum where
  isMatch :: enum -> enumCtr enumValue -> Boolean

class Enum :: forall k1 k2. (k1 -> Type) -> k1 -> k2 -> Constraint
class Enum p curr next | p curr -> next where
  enumValue :: p curr

class EnumConfig :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class EnumConfig x y z | x -> y z

class MatchHelper :: forall k1 k2. k1 -> k2 -> Type -> Type -> Type -> Constraint
class MatchHelper p enumValue input output fun | p enumValue input output -> fun where
  matchHelp :: Proxy p -> Proxy enumValue -> Proxy input -> output -> (input -> (input -> output) -> output) -> fun

instance MatchHelper p Unit input output (input -> output) where
  matchHelp _ _ _ output cont = flip cont (const output)
else instance (IsMatch input p enumValue, Enum p enumValue nextEnumValue, MatchHelper p nextEnumValue input output nextResult) => MatchHelper p enumValue input output (((p enumValue -> output) -> nextResult)) where
  matchHelp _ _ _ output cont =
    let
      handler :: (p enumValue -> output) -> nextResult
      handler applyMatch =
        let
          nextCont input otherwise =
            cont input
              $ \i ->
                  let
                    matchEnumValue :: p enumValue
                    matchEnumValue = enumValue
                  in
                    if (isMatch i matchEnumValue) then
                      applyMatch matchEnumValue
                    else
                      otherwise i

          nextResult :: nextResult
          nextResult = matchHelp (Proxy :: Proxy p) (Proxy :: Proxy nextEnumValue) (Proxy :: Proxy input) output nextCont
        in
          nextResult
    in
      handler

class Match :: forall k. k -> Type -> Type -> Constraint
class Match input output result | input output -> result where
  match :: Proxy input -> output -> result

instance (EnumConfig input enumCtr enumValue, MatchHelper enumCtr enumValue input output fun) => Match input output fun where
  match :: Proxy input -> output -> fun
  match inputProxy output = matchHelp (Proxy :: Proxy enumCtr) (Proxy :: Proxy enumValue) inputProxy output applyFlipped
