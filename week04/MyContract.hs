{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- ^ Add extensions on top of the module file.
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.MyContract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet


import Ledger
import Ledger.Value         as Value
import Ledger.TimeSlot
import Ledger.Ada                 as Ada
import qualified Data.Map                   as Map
import           Data.Default               (Default (..))

myContract :: Contract () Empty Text ()
myContract = do
  void $ Contract.throwError "BOOM!"
  Contract.logInfo @String "Hello from the contract!"

myTrace :: EmulatorTrace ()
myTrace = void $ activateContractWallet (knownWallet 1) myContract
-- myTrace = h <- activateContractWallet (Wallet 1) myContract

test :: IO ()
test = runEmulatorTraceIO myTrace

myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
  (\err -> Contract.logError $ "Caught error: " ++ unpack err) 
  myContract

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2
-- myTrace = h <- activateContractWallet (Wallet 1) myContract

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
  h1 <- activateContractWallet (knownWallet 1) myContract3
  callEndpoint @"foo" h1 123

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- The Writer parameter: w
-- This type parameter can not be of any type but an instance of the type class Monoid. An example of data type which is an instance of this class is List. This parameter of the Contract monad is essential because it allows us to bring information back from the contract to the trace and also to the PAB, the Plutus Application Backend. We will be able to pass info back from the contract running in the wallet to the outside world.

-- if you are asking how can it return an empty list, just remember that we imposed that the Writer parameter type had to be an instance of the type class Monoid. This type class implements three functions, the first one being mempty :: a which just gives you the empty object of your data type instance. In this case, our data type instance is a List, so the empty object is [].

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
    Extras.logInfo $ "my log info: " ++ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4

myContract5 :: Contract [String] Empty Text ()
myContract5 = do
    void $ Contract.waitNSlots 10
    tell ["first communication from contract"]
    void $ Contract.waitNSlots 10
    tell ["second com from contract"]
    void $ Contract.waitNSlots 10

myTrace5 :: EmulatorTrace ()
myTrace5 = do
    h <- activateContractWallet (knownWallet 1) myContract5

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
    Extras.logInfo $ "my log info: " ++ show zs

test5 :: IO ()
test5 = runEmulatorTraceIO myTrace5



-- custom emulator config
-- test6 :: IO ()
-- test = runEmulatorTraceIO' def emCfg myTrace
--   where
--     emCfg :: EmulatorConfig
--     emCfg = EmulatorConfig $ Left $ Map.fromList [
--                               (Wallet 1, v <> Value.singleton assetSymbol assetToken 1)
--                             , (Wallet 2, v)
--                             , (Wallet 3, v)]

--     v :: Value
--     v = Ada.lovelaceValueOf 100_000_000
--     -- {-# LANGUAGE NumericUnderscores    #-}



