# crypto-keys-ssh

This is just like
[crypto-pubkey-openssh](https://github.com/knsd/crypto-pubkey-openssh)
but not dependent on crypto-pubkey-types nor any specific crypto
library.

Can be easily used with
[cryptonite](https://github.com/haskell-crypto/cryptonite) like so:

```haskell
import Crypto.PubKey.SSH (decodeSshPubKey)
import Crypto.PubKey.RSA (PublicKey (..))
import Data.ByteString.Lazy.Char8 as LBS

decodeSshPubKey PublicKey <$> LBS.readFile ".ssh/id_rsa.pub"
```
