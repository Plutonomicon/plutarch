module Plutarch.Api.Internal.Scripts (
  hashScriptWithPrefix,
) where

import Codec.Serialise (serialise)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (
  Blake2b_224 (Blake2b_224),
 )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx.Builtins as PlutusTx

-- | Hash a Script with the given version prefix
hashScriptWithPrefix :: ByteString -> Plutus.Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash $
    PlutusTx.toBuiltin $
      convert @_ @ByteString $
        hashWith Blake2b_224 $
          prefix <> (Lazy.toStrict $ serialise scr)
