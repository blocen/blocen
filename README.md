### Plutus Pioneer Program | 3P C3

https://cabal.readthedocs.io/en/latest/getting-started.html

https://www.cis.upenn.edu/~cis194/spring13/lectures.html

#### Week #1

Enter a nix-shell.
Build the English Auction contract with cabal build (you may need to run cabal update first).
Go to the plutus-playground-client folder in the plutus-apps repository.
Start the Playground server with plutus-playground-server.
Start the Playground client (in another nix-shell) with npm start.
Copy-paste the auction contract into the Playground editor, do not forget to remove the module header.
Compile.

#### Week #2

a. Fix and complete the code in the [Homework1](code/week02/src/Week02/Homework1.hs) module.

b. Fix and complete the code in the [Homework2](code/week02/src/Week02/Homework2.hs) module.

#### Week #3


#### Week #4

#### Playground mocks

cabal repl

-----------------------------------------------------
-- to determine wallet info

import Wallet.Emulator

mockWalletPaymentPubKeyHash $ knownWallet 1
-- a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2

mockWalletPaymentPubKeyHash $ knownWallet 2
-- 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7

mockWalletPaymentPubKeyHash $ knownWallet 3
-- 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c

-----------------------------------------------------
-- to determine POSIX time of slots in the playground

import Ledger.Time
import Ledger.TimeSlot
import Data.Default

slotToBeginPOSIXTime def 10
-- 1596059101000
slotToBeginPOSIXTime def 20
-- 1596059111000


#### Some Plutus Modules

- `Ledger.Scripts`, contains functions related to untyped Plutus scripts.
- `Ledger.Typed.Scripts`, contains functions related to typed Plutus scripts.
- `Plutus.V1.Ledger.Interval`, contains the definition of intervals and functions for working with them.
- `Plutus.V1.Ledger.Time`, contains time-related types and functions.
- `PlutusTx`, contains important types like `Data` and `BuiltinData`.
- `PlutusTx.IsData.Class`, contains the `ToData` and `FromData` classes and related functions.
- `Wallet.Emulator`, contains types and functions for dealing with wallets.

#### Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [The Plutus-Apps repository](https://github.com/input-output-hk/plutus-apps)
- Learn You a Haskell for Great Good: [original](http://learnyouahaskell.com/),
  [remastered](https://hansruec.github.io/learn-you-a-haskell-remastered/01-first-things-first.html) and
  [interactive notebook](https://hub.gke2.mybinder.org/user/jamesdbrock-lea-askell-notebook-24dgdx7w/lab/tree/learn_you_a_haskell/00-preface.ipynb)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)
