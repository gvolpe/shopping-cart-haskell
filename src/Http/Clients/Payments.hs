module Http.Clients.Payments where

import           Domain.Payment

data PaymentClient m = PaymentClient
  { processPayment :: Payment -> m PaymentId
  }
