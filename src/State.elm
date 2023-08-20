module State exposing (..)

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


type alias EmergencyFundPlan =
    { maxAmount : Float
    , maxAmountAsString : String
    , percentageToApply : Float
    , percentageToApplyAsString : String
    }


type alias Payment =
    Float


type alias EmergencyFundPayments =
    { plan : EmergencyFundPlan
    , payments : List Payment
    }


type alias StrategyForm =
    { maxNumberOfYears : String
    , maxTotalPayment : String
    , paymentStrategy : PaymentStrategy
    , emergencyFund : Maybe EmergencyFundPlan
    }


type alias Loan =
    { name : String
    , apr : Float
    , minimum : Float
    , principal : Float
    }


type alias PaymentSequence =
    { loan : Loan
    , actualMinimum : Float
    , payments : List Payment
    , isPaidOff : Bool
    }


type alias PaymentPlan =
    { payments : List PaymentSequence
    , savings : Maybe EmergencyFundPayments
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
    | LoadFile
