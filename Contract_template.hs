{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NamedFieldPuns      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- A module starts with its export declarations of symbols declared in this file.
-- module MyModule (myExport1, myExport2) where
module Contract_template where

-- Followed by a set of imports of symbols from other files
-- import OtherModule (myImport1, myImport2)

-- Import all symbols into the local namespace.
-- import Data.List

-- Import select symbols into the local namespace:
-- import Data.List (nub, sort)

-- We can import one or more constructors explicitly:
-- import Text.Read (Lexeme(Ident, Symbol))

-- All constructors for a given type can also be imported:
-- import Text.Read (Lexeme(..))

-- We can also import types and classes defined in the module:
-- import Text.Read (Read, ReadS)

-- Import into the global namespace masking a symbol:
-- import Data.List hiding (nub)

-- Import symbols qualified under Data.Map namespace into the local namespace.
-- import qualified Data.Map

-- A second form does not create an alias. Instead,
-- the prefix becomes the module name. We can
-- write a simple function to check if a string is all
-- upper case:
-- import qualified Char
-- allUpper str = all Char.isUpper str

-- Import symbols qualified and reassigned to a custom namespace (M, in the example below):
-- import qualified Data.Map as M

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO)
import qualified Prelude              as P
import           Text.Printf          (printf)

-- data types
-- data Point = Point Int Int
-- data Point2 = Point2 { x :: Int, y :: Int }
data VestingDatum = VestingDatum
    { beneficiary1 :: PaymentPubKeyHash
    , beneficiary2 :: PaymentPubKeyHash
    , deadline     :: POSIXTime
    } deriving P.Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
-- mkValidator dat () ctx
--     | (unPaymentPubKeyHash (beneficiary1 dat) `elem` sigs) && (to       (deadline dat) `contains` range) = True
--     | (unPaymentPubKeyHash (beneficiary2 dat) `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
--     | otherwise                                                                                          = False
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     sigs :: [PubKeyHash]
--     sigs = txInfoSignatories info

--     range :: POSIXTimeRange
--     range = txInfoValidRange info

-- needs {-# LANGUAGE NamedFieldPuns #-} for b1, b1, deadline
mkValidator VestingDatum{beneficiary1, beneficiary2, deadline} _ ScriptContext{scriptContextTxInfo=txInfo} =
    let signedBy1  = txInfo `txSignedBy` unPaymentPubKeyHash beneficiary1
        signedBy2  = txInfo `txSignedBy` unPaymentPubKeyHash beneficiary2
        vr         = txInfoValidRange txInfo
    in if from deadline `contains` vr
        then traceIfFalse "tx unsigned by beneficiary 1" signedBy1
        else traceIfFalse "tx unsigned by beneficiary 2" signedBy2

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    pkh <- ownPaymentPubKeyHash
    let dat = VestingDatum
                { beneficiary1 = gpBeneficiary gp
                , beneficiary2 = pkh
                , deadline     = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (P.show $ gpBeneficiary gp)
        (P.show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now    <- currentTime
    pkh    <- ownPaymentPubKeyHash
    utxos  <- utxosAt scrAddress
    let utxos1 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
        utxos2 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now >  deadline dat) utxos
    logInfo @P.String $ printf "found %d gift(s) to grab" (Map.size utxos1 P.+ Map.size utxos2)
    unless (Map.null utxos1) $ do
        let orefs   = fst <$> Map.toList utxos1
            lookups = Constraints.unspentOutputs utxos1 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (to now)
        void $ submitTxConstraintsWith @Void lookups tx
    unless (Map.null utxos2) $ do
        let orefs   = fst <$> Map.toList utxos2
            lookups = Constraints.unspentOutputs utxos2 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref $ unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (from now)
        void $ submitTxConstraintsWith @Void lookups tx
  where
    isSuitable :: (VestingDatum -> Bool) -> ChainIndexTxOut -> Bool
    isSuitable p o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum d) -> maybe False p $ PlutusTx.fromBuiltinData d

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []



-- todo: emulator trace
-- todo: customised em config

-- http://dev.stephendiehl.com/hask/index.html

-- data Suit = Clubs | Diamonds | Hearts | Spades
-- data Color = Red | Black
-- data Value 
--   = Two
--   | Three
--   | Four
--   | Five
--   | Six
--   | Seven
--   | Eight
--   | Nine
--   | Ten 
--   | Jack
--   | Queen
--   | King
--   | Ace
--   deriving (Eq, Ord)

-- data Card = Card
--   { suit  :: Suit
--   , color :: Color
--   , value :: Value
--   }

-- queenDiamonds :: Card
-- queenDiamonds = Card Diamonds Red Queen

-- -- Alternatively 
-- queenDiamonds :: Card
-- queenDiamonds = Card { suit = Diamonds, color = Red, value = Queen }

-- -- The convention for preventing these kind of values in Haskell is to limit the export of constructors in a module and only provide a limited set of functions which the module exports, which can enforce these constraints. These are smart constructors 
-- module Cards (Card, diamond, spade, heart, club) where

-- diamond :: Value -> Card
-- diamond = Card Diamonds Red

-- spade :: Value -> Card
-- spade = Card Spades Black

-- heart :: Value -> Card
-- heart = Card Hearts Red

-- club :: Value -> Card
-- club = Card Clubs Black


-- -- traverse a list
-- addOne :: [Int] -> [Int]
-- addOne (x : xs) = (x+1) : (addOne xs)
-- addOne [] = []


-- guards
-- absolute :: Int -> Int
-- absolute n 
--   | n < 0     = (-n)
--   | otherwise = n

-- Guards can also occur in pattern case expressions.
-- absoluteJust :: Maybe Int -> Maybe Int
-- absoluteJust n = case n of
--   Nothing -> Nothing
--   Just n
--     | n < 0     -> Just (-n)
--     | otherwise -> Just n


-- absolute :: Int -> Int
-- absolute n = 
--   if (n < 0) 
--     then (-n) 
--     else n
-- If statements are just syntactic sugar for case expressions over boolean values. The following example is equivalent to the above example.

-- absolute :: Int -> Int
-- absolute n = case (n < 0) of 
--   True  -> (-n)
--   False -> n

-- function application operator $. 
-- This function is right associative and takes the entire expression on the right hand side of the operator and applies it to a function on the left.

-- infixr 0 $
-- ($) :: (a -> b) -> a -> b 

-- Function Composition .
-- operation which takes two functions and produces another function with the result of the first argument function applied to the result of the second function.
-- example :: [Integer] -> [Integer]
-- example =
--     sort -- sort is in import Data.List
--   . filter (<100)
--   . map (*10)


