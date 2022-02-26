module State exposing (..)

import Loan exposing (Loan)


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
