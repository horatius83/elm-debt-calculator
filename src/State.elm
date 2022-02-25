module State exposing (..)


type alias Loan =
    { name : String
    , apr : Float
    , minimum : Float
    , principal : Float
    }


type alias Model =
    { loans : List Loan
    , newLoan : Loan
    , errors : List String
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
