module MainTests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (..)

import State exposing (Model, PaymentStrategy(..), StrategyForm)
import Main exposing (generatePaymentPlan)
import Loan exposing (Loan)
import NewLoan exposing (emptyLoanForm)
import State exposing (FormState(..))

suite : Test
suite = 
    describe "The Main Module"
        [ test "generatePaymentPlan" <|
            \_ -> 
                let
                    yearsToPayoff = 20
                    loan = Loan "Test 1" 10.0 10.0 5000
                    loans = [loan]
                    paymentStrategy = Avalanche
                    strategyForm = StrategyForm "20" "500"
                    model = Model loans [] yearsToPayoff paymentStrategy 500.0 Nothing Nothing Nothing EnterLoans  emptyLoanForm strategyForm
                in
                generatePaymentPlan model 
                    |> Tuple.first 
                    |> (\x -> x.paymentPlan)
                    |> Maybe.map List.head
                    |> Maybe.map (\x -> Maybe.withDefault 0 (Maybe.map (\y -> List.length y.payments) x))
                    |> Maybe.withDefault 0
                    |> Expect.greaterThan 1
        ]
