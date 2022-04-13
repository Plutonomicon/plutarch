module Plutarch.Rational (
  PRational (..),
  preduce,
  pnumerator,
  pdenominator,
  pratFromInt,
  pround,
  ptruncate,
  pproperFraction,
  (#%),
) where

import Data.Ratio (denominator, numerator)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PlutusType (..),
  Term,
  pcon,
  pfix,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PEq (..), POrd (..), pif)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData (..),
  pasInt,
  pasList,
  pforgetData,
 )
import Plutarch.Integer (
  PInteger,
  pdiv,
  pmod,
  pquot,
  prem,
 )
import Plutarch.List (PListLike (pcons, phead, pnil, ptail), pmap)
import Plutarch.Pair (PPair (..))
import Plutarch.Show (PShow (pshow'), pshow)
import Plutarch.Trace (ptraceError)
import Plutarch.Unsafe (punsafeCoerce)

data PRational s
  = PRational (Term s PInteger) (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)

instance PShow PRational where
  pshow' _ x =
    pshowRat # x
    where
      pshowRat = phoistAcyclic $
        plam $ \n -> pmatch n $ \(PRational x y) ->
          pshow x <> "/" <> pshow y

instance PIsData PRational where
  pfromData x' =
    phoistAcyclic
      ( plam $ \x ->
          pListToRat #$ pmap # pasInt #$ pasList # pforgetData x
      )
      # x'
  pdata x' =
    phoistAcyclic
      ( plam $ \x ->
          ( punsafeCoerce ::
              Term _ (PAsData (PBuiltinList (PAsData PInteger))) ->
              Term _ (PAsData PRational)
          )
            $ pdata $ pRatToList # x
      )
      # x'

pRatToList :: Term s (PRational :--> PBuiltinList (PAsData PInteger))
pRatToList = plam $ \x -> pmatch x $ \(PRational a b) ->
  pcons # pdata a #$ pcons # pdata b #$ punsafeCoerce (pnil :: Term s (PBuiltinList PData))

pListToRat :: Term s (PBuiltinList PInteger :--> PRational)
pListToRat = plam $ \x -> pcon $ PRational (phead # x) (phead #$ ptail # x)

instance PEq PRational where
  l' #== r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #== rn * ld
      )
      # l'
      # r'

instance POrd PRational where
  l' #<= r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #<= rn * ld
      )
      # l'
      # r'

  l' #< r' =
    phoistAcyclic
      ( plam $ \l r ->
          pmatch l $ \(PRational ln ld) ->
            pmatch r $ \(PRational rn rd) ->
              rd * ln #< rn * ld
      )
      # l'
      # r'

instance Num (Term s PRational) where
  x' + y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce #$ pmatch x $
            \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yd + yn * xd) (xd * yd)
      )
      # x'
      # y'

  x' - y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yd - yn * xd) (xd * yd)
      )
      # x'
      # y'

  x' * y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                pcon $ PRational (xn * yn) (xd * yd)
      )
      # x'
      # y'

  negate x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            pcon $ PRational (negate xn) xd
      )
      # x'

  abs x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            pcon $ PRational (abs xn) (abs xd)
      )
      # x'

  signum x'' =
    phoistAcyclic
      ( plam $ \x' -> plet x' $ \x ->
          pif
            (x #== 0)
            0
            $ pif
              (x #< 0)
              (-1)
              1
      )
      # x''

  fromInteger n = pcon $ PRational (fromInteger n) 1

instance Fractional (Term s PRational) where
  recip x' =
    phoistAcyclic
      ( plam $ \x ->
          pmatch x $ \(PRational xn xd) ->
            pfailOnZero # xn # (pcon (PRational xd xn))
      )
      # x'

  x' / y' =
    phoistAcyclic
      ( plam $ \x y ->
          preduce
            #$ pmatch x
            $ \(PRational xn xd) ->
              pmatch y $ \(PRational yn yd) ->
                plet (xd * yn) $ \den ->
                  pfailOnZero # den
                    #$ pcon (PRational (xn * yd) den)
      )
      # x'
      # y'

  fromRational r =
    pcon $ PRational (fromInteger $ numerator r) (fromInteger $ denominator r)

pfailOnZero :: Term s (PInteger :--> a :--> a)
pfailOnZero = phoistAcyclic $
  plam $ \n x ->
    pif
      (n #== 0)
      (ptraceError "pfailOnZero: division by 0")
      x

preduce :: Term s (PRational :--> PRational)
preduce = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational xn xd) ->
      plet (pgcd # xn # xd) $ \r ->
        plet (signum xd) $ \s ->
          pcon $ PRational (s * pdiv # xn # r) (s * pdiv # xd # r)

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' ->
    plet (abs x') $ \x ->
      plet (abs y') $ \y ->
        plet (pmax # x # y) $ \a ->
          plet (pmin # x # y) $ \b ->
            pgcd' # a # b

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = phoistAcyclic $ pfix #$ plam $ f
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

pmax :: POrd a => Term s (a :--> a :--> a)
pmax = phoistAcyclic $ plam $ \a b -> pif (a #<= b) b a

pnumerator :: Term s (PRational :--> PInteger)
pnumerator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational n _) -> n

pdenominator :: Term s (PRational :--> PInteger)
pdenominator = phoistAcyclic $ plam $ \x -> pmatch x $ \(PRational _ d) -> d

pratFromInt :: Term s (PInteger :--> PRational)
pratFromInt = phoistAcyclic $ plam $ \n -> pcon $ PRational n 1

pratFromIntsUnsafe :: Term s (PInteger :--> PInteger :--> PRational)
pratFromIntsUnsafe =
  phoistAcyclic $ plam $ \n d -> pcon $ PRational n d

(#%) :: Term s PInteger -> Term s PInteger -> Term s PRational
n #% d = pratFromIntsUnsafe # n # d

pround :: Term s (PRational :--> PInteger)
pround = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) ->
      plet (pdiv # a # b) $ \base ->
        plet (pmod # a # b) $ \rem ->
          base
            + pif
              (pmod # b # 2 #== 1)
              (pif (pdiv # b # 2 #< rem) 1 0)
              ( pif
                  (pdiv # b # 2 #== rem)
                  (pmod # base # 2)
                  (pif (rem #< pdiv # b # 2) 0 1)
              )

ptruncate :: Term s (PRational :--> PInteger)
ptruncate = phoistAcyclic $
  plam $ \x ->
    pmatch x $ \(PRational a b) -> pquot # a # b

pproperFraction :: Term s (PRational :--> PPair PInteger PRational)
pproperFraction =
  phoistAcyclic $
    plam $ \x ->
      pmatch x $ \(PRational n d) ->
        pcon $
          PPair
            (pquot # n # d)
            (pcon $ PRational (prem # n # d) d)
