module Plutarch.Extra.List (preverse, pcheckSorted) where

import Plutarch.Prelude

-- | / O(n) /. reverses a list
preverse :: (PIsListLike l a) => Term s (l a #-> l a)
preverse =
  phoistAcyclic $
    pfoldl # plam (\ys y -> pcons # y # ys) # pnil

-- | / O(n) /.checks whether a list is sorted
pcheckSorted :: (PIsListLike l a, POrd a) => Term s (l a #-> PBool)
pcheckSorted =
  pfix #$ plam $ \self xs ->
    pelimList
      ( \x1 xs ->
          pelimList
            (\x2 _ -> x1 #<= x2 #&& (self # xs))
            (pcon PTrue)
            xs
      )
      (pcon PTrue)
      xs
