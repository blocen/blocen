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
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wall #-}

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


-- List Comprehensions

-- Generators
-- Let bindings 
-- Guards
-- [n*x | x <- [1,2,3,4,5], let n = 3, odd x]

-- cartesian product
-- [(x,y) | x <- [1,2,3], y <- [10,20,30]]

-- [ e1.. ]	enumFrom e1
-- [ e1,e2.. ]	enumFromThen e1 e2
-- [ e1..e3 ]	enumFromTo e1 e3
-- [ e1,e2..e3 ]	enumFromThenTo e1 e2 e3

-- fizzbuzz :: [String]
-- fizzbuzz = [fb x| x <- [1..100]]
--     where fb y
--         | y `mod` 15 == 0 = "FizzBuzz"
--         | y `mod` 3  == 0 = "Fizz"
--         | y `mod` 5  == 0 = "Buzz"
--         | otherwise  = show y



-- The undefined function is extremely practical for debugging or to accommodate writing incomplete programs.

-- undefined :: a

-- mean :: Num a => Vector a -> a
-- mean nums = (total / count) where            -- Partially defined function
--               total = undefined
--               count = undefined

-- addThreeNums :: Num a => a -> a -> a -> a
-- addThreeNums n m j = undefined               -- No function body declared at all

-- f :: a -> Complicated Type
-- f = undefined                                -- Write tomorrow, typecheck today!
--                                              -- Arbitrarily complicated types
--                                              -- welcome!

-- Haddock
-- methods uses -- | to delineate the beginning of a comment:

-- -- | Documentation for f
-- f :: a -> a
-- f = ...
-- Multiline comments are also possible:

-- -- | Multiline documentation for the function
-- -- f with multiple arguments.
-- fmap :: Functor f
--      => (a -> b)  -- ^ function
--      -> f a       -- ^ input
--      -> f b       -- ^ output
-- -- ^ is used to comment Constructors or Record fields:

-- data T a b
--   = A a -- ^ Documentation for A
--   | B b -- ^ Documentation for B

-- data R a b = R
--   { f1 :: a -- ^ Documentation for the field f1
--   , f2 :: b -- ^ Documentation for the field f2
--   }
-- Elements within a module (i.e. values, types, classes) can be hyperlinked by enclosing the identifier in single quotes:

-- data T a b
--   = A a -- ^ Documentation for 'A'
--   | B b -- ^ Documentation for 'B'
-- Modules themselves can be referenced by enclosing them in double quotes:

-- -- | Here we use the "Data.Text" library and import
-- -- the 'Data.Text.pack' function.
-- haddock also allows the user to include blocks of code within the generated documentation. Two methods of demarcating the code blocks exist in haddock. For example, enclosing a code snippet in @ symbols marks it as a code block:

-- -- | An example of a code block.
-- --
-- -- @
-- --    f x = f (f x)
-- -- @
-- Similarly, it is possible to use bird tracks (>) in a comment line to set off a code block.

-- -- | A similar code block example that uses bird tracks (i.e. '>')
-- -- > f x = f (f x)
-- Snippets of interactive shell sessions can also be included in haddock documentation. In order to denote the beginning of code intended to be run in a REPL, the >>> symbol is used:

-- -- | Example of an interactive shell session embedded within documentation
-- --
-- -- >>> factorial 5
-- -- 120
-- Headers for specific blocks can be added by prefacing the comment in the module block with a *:

-- module Foo (
--   -- * My Header
--   example1,
--   example2
-- )
-- Sections can also be delineated by $ blocks that pertain to references in the body of the module:

-- module Foo (
--   -- $section1
--   example1,
--   example2
-- )

-- -- $section1
-- -- Here is the documentation section that describes the symbols
-- -- 'example1' and 'example2'.
-- Links can be added with the following syntax:

-- <url text>
-- Images can also be included, so long as the path is either absolute or relative to the directory in which haddock is run.

-- <<diagram.png title>>
-- haddock options can also be specified with pragmas in the source, either at the module or project level.

-- {-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
-- Option	Description
-- ignore-exports	Ignores the export list and includes all signatures in scope.
-- not-home	Module will not be considered in the root documentation.
-- show-extensions	Annotates the documentation with the language extensions used.
-- hide	Forces the module to be hidden from Haddock.
-- prune	Omits definitions with no annotations.


