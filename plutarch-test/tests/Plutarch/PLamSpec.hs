module Plutarch.PLamSpec (spec) where

import Data.ByteString (ByteString)
import qualified PlutusCore as PLC

import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Unsafe (punsafeBuiltin)
import Test.Hspec

spec :: Spec
spec = do
  describe "plam" . pgoldenSpec $ do
    "id" @| plam (\x -> x)
    "flip.const" @| plam (\_ y -> y)
    "plet" @| plam (\x _ -> plet x $ \_ -> perror)
    "primitives" @\ do
      "bool" @\ do
        "true" @| plam $ \_ -> pconstant True
      "int" @\ do
        "0" @| plam $ \_ -> (0 :: Term _ PInteger)
        "1" @| plam $ \_ -> (1 :: Term _ PInteger)
        "512" @| plam $ \_ -> (512 :: Term _ PInteger)
        "1048576" @| plam $ \_ -> (1048576 :: Term _ PInteger)
      "bytestring" @\ do
        "1" @| plam $ \_ -> pconstant ("1" :: ByteString)
        "1111111" @| plam $ \_ -> pconstant ("1111111" :: ByteString)
      "unit" @\ do
        "list" @| plam $ \_ -> pconstant ([()] :: [()])
        "()" @| plam $ \_ -> pconstant ()
      "id" @| plam $ \x -> x
      "fun" @\ do
        "lam+" @| plam $ \_ -> (plam (+) :: Term _ (PInteger #-> PInteger #-> PInteger))
        "+" @| (plam (+) :: Term _ (PInteger #-> PInteger #-> PInteger))
    "η-reduction-optimisations" @\ do
      "λx y. addInteger x y => addInteger"
        @| plam
        $ \x y -> (x :: Term _ PInteger) + y
      "λx y. hoist (force mkCons) x y => force mkCons"
        @| plam
        $ \x y -> (pforce $ punsafeBuiltin PLC.MkCons) # x # y
      "λx y. hoist mkCons x y => mkCons x y"
        @| plam
        $ \x y -> (punsafeBuiltin PLC.MkCons) # x # y
      "λx y. hoist (λx y. x + y - y - x) x y => λx y. x + y - y - x"
        @| plam
        $ \x y -> (phoistAcyclic $ plam $ \(x :: Term _ PInteger) y -> x + y - y - x) # x # y
      "λx y. x + x"
        @| plam
        $ \(x :: Term _ PInteger) (_ :: Term _ PInteger) -> x + x
      "let x = addInteger in x 1 1"
        @| plet (punsafeBuiltin PLC.AddInteger)
        $ \x -> x # (1 :: Term _ PInteger) # (1 :: Term _ PInteger)
      "let x = 0 in x => 0"
        @| plet 0
        $ \(x :: Term _ PInteger) -> x
      "let x = hoist (λx. x + x) in 0 => 0"
        @| plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x)
        $ \_ -> (0 :: Term _ PInteger)
      "let x = hoist (λx. x + x) in x"
        @| plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x)
        $ \x -> x
      "λx y. sha2_256 x y =>!"
        @| (plam $ \x y -> punsafeBuiltin PLC.Sha2_256 # x # y)
      "let f = hoist (λx. x) in λx y. f x y => λx y. x y"
        @| (plam $ \x y -> (phoistAcyclic $ plam $ \x -> x) # x # y)
      "let f = hoist (λx. x True) in λx y. f x y => λx y. (λz. z True) x y"
        @| (plam $ \x y -> ((phoistAcyclic $ plam $ \x -> x # pcon PTrue)) # x # y)
      "λy. (λx. x + x) y"
        @| plam
        $ \y -> (plam $ \(x :: Term _ PInteger) -> x + x) # y
