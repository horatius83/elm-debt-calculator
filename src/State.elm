module State exposing (..)

import Loan exposing (Loan, PaymentPlan)
import Time


type PaymentStrategy
    = Avalanche
    | Snowball


type alias Model =
    { loans : List Loan
    , newLoan : Loan
    , errors : List String
    , yearsToPayoff : Int
    , paymentStrategy : PaymentStrategy
    , totalMonthlyPayment : Float
    , paymentPlan : Maybe PaymentPlan
    , currentTime : Maybe Time.Posix
    , currentTimeZone : Maybe Time.Zone
    }


type Msg
    = AddLoan
    | ResetNewLoan
    | DeleteLoan Int
    | UpdateLoanName String
    | UpdateLoanApr String
    | UpdateLoanPrincipal String
    | UpdateLoanMinimum String
    | Error String
    | DoNothing
    | UpdateYearsToPayoff String
    | UpdateTime Time.Posix
    | UpdateTimeZone Time.Zone
    | ChoosePaymentStrategy PaymentStrategy
    | UpdateMaximumTotalPayment String
    | GeneratePaymentPlan
