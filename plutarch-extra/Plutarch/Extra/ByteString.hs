module Plutarch.Extra.ByteString (pallBS, pisHexDigit) where

import Plutarch.Prelude

pallBSPPlutus' s => Term s ((PInteger #-> PBool) #-> PByteString #-> PBool)
pallBS = phoistAcyclic $
  plam $ \f str -> plet (plengthBS # str) $ \ln ->
    let helper :: Term _ (PInteger #-> PBool)
        helper = pfix #$ plam $ \self i ->
          pif
            (i #< ln)
            ( pif
                (f #$ pindexBS # str # i)
                (self # (i + 1))
                (pcon PFalse)
            )
            (pcon PTrue)
     in helper # 0

pisHexDigitPPlutus' s => Term s (PInteger #-> PBool)
pisHexDigit = phoistAcyclic $
  plam $ \chr ->
    (chr #<= 57 #&& 48 #<= chr)
      #|| (chr #<= 70 #&& 65 #<= chr)
      #|| (chr #<= 102 #&& 97 #<= chr)
