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

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
