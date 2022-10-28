module Plutarch.Api.V1.Tuple (PTuple, ptuple, ptupleFromBuiltin, pbuiltinPairFromTuple) where

import Plutarch.Builtin (pconstrBuiltin, pforgetData)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

type PTuple a b =
  PDataSum
    '[ '[ "_0" ':= a
        , "_1" ':= b
        ]
     ]

ptuplePPlutus' s => Term s (PAsData a #-> PAsData b #-> PTuple a b)
ptuple = phoistAcyclic $
  plam $ \x y ->
    let target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
        target = pconstrBuiltin # 0 #$ pcons # pforgetData x #$ pcons # pforgetData y # pnil
     in punsafeCoerce target

ptupleFromBuiltinPPlutus' s => Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b))) -> Term s (PAsData (PTuple a b))
ptupleFromBuiltin = punsafeCoerce

pbuiltinPairFromTuplePPlutus' s => Term s (PAsData (PTuple a b)) -> Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b)))
pbuiltinPairFromTuple = punsafeCoerce
