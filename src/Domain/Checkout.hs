{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Domain.Checkout where

import           Control.Monad                  ( unless )
import           Control.Monad.Catch            ( Exception )
import           Control.ParDual.Class
import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Typeable                  ( typeOf )
import           Data.UUID                      ( UUID )
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( KnownNat
                                                , Nat
                                                , natVal'
                                                )
import           Refined
import           Refined.Helper                 ( i2text
                                                , nv
                                                , ref
                                                )
import           Refined.Instances              ( )

data OrderError = OrderError deriving (Exception, Show)
data PaymentError = PaymentError deriving (Exception, Show)

data HasDigits (n :: Nat) = HasDigits deriving Generic

instance (Integral x, Show x, KnownNat n) => Predicate (HasDigits n) x where
  validate p x = do
    let n = fromIntegral (nv @n)
    if n == toInteger (length $ show x)
      then Nothing
      else throwRefineOtherException
        (typeOf p)
        ("Invalid number of digits. Expected " <> i2text n)

type CardNamePred = Refined NonEmpty Text
type CardNumberPred = Refined (HasDigits 16) Int
type CardExpirationPred = Refined (HasDigits 4) Int
type CardCVVPred = Refined (HasDigits 3) Int

newtype CardName = CardName CardNamePred deriving (Generic, Show)
newtype CardNumber = CardNumber CardNumberPred deriving (Generic, Show)
newtype CardExpiration = CardExpiration CardExpirationPred deriving (Generic, Show)
newtype CardCVV = CardCVV CardCVVPred deriving (Generic, Show)

data Card = Card
  { cardName :: CardName
  , cardNumber :: CardNumber
  , cardExpiration :: CardExpiration
  , cardCVV :: CardCVV
  } deriving (Generic, Show)

validateCard :: Text -> Int -> Int -> Int -> Either [String] Card
validateCard n r e c = parMap4 (CardName <$> ref n "name")
                               (CardNumber <$> ref r "number")
                               (CardExpiration <$> ref e "expiration")
                               (CardCVV <$> ref c "cvv")
                               Card

instance FromJSON Card where
  -- This needs to return a Parser Card and there is not much we can do with it
  parseJSON = withObject "Card json" $ \o -> do
    n <- o .: "name"
    r <- o .: "number"
    e <- o .: "expiration"
    c <- o .: "cvv"
    case validateCard n r e c of
      Left  err  -> fail $ show err
      Right card -> return card

instance ToJSON Card where
  toJSON (Card (CardName name) (CardNumber number) (CardExpiration exp) (CardCVV cvv))
    = object
      ["name" .= name, "number" .= number, "expiration" .= exp, "cvv" .= cvv]
