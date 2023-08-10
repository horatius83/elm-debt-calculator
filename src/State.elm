module State exposing (..)

import Loan exposing (EmergencyFundPlan, Loan, PaymentPlan)
import Time


type PaymentStrategy
    = Avalanche
    | Snowball


type FormState
    = EnterLoans
    | EnterPaymentStrategy
    | ViewPaymentPlan


type alias NewLoanForm =
    { name : String
    , principal : String
    , minimum : String
    , apr : String
    }


type alias StrategyForm =
    { maxNumberOfYears : String
    , maxTotalPayment : String
    , paymentStrategy : PaymentStrategy
    , emergencyFund : Maybe EmergencyFundPlan
    }


type alias Model =
    { loans : List Loan
    , errors : List String
    , yearsToPayoff : Int
    , totalMonthlyPayment : Float
    , paymentPlan : Maybe PaymentPlan
    , currentTime : Maybe Time.Posix
    , currentTimeZone : Maybe Time.Zone
    , formState : FormState
    , newLoanForm : NewLoanForm
    , strategyForm : StrategyForm
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
    | UpdateTimeAndThen (Time.Posix -> Time.Zone -> ( Model, Cmd Msg )) Time.Posix
    | UpdateTimeZoneAndThen (Time.Zone -> ( Model, Cmd Msg )) Time.Zone
    | ChoosePaymentStrategy PaymentStrategy
    | UpdateMaximumTotalPayment String
    | GeneratePaymentPlan
    | GeneratePaymentPlanAsPdf
    | ChangeFormState FormState
    | ToggleHasEmergencyFund
    | UpdateEmergencyFundMaxAmount String
    | UpdateEmergencyFundPercentage String
