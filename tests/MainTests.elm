module MainTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import State exposing (Model, PaymentStrategy)
import Main exposing (generatePaymentPlan)
import Loan exposing (toPaymentPlan, Loan)
import State exposing (PaymentStrategy(..))

suite : Test
suite = 
    describe "The Main Module"
        [ test "generatePaymentPlan" <|
            \_ -> 
                let
                    yearsToPayoff = 20
                    emptyLoan = Loan "" 0 0 0
                    loan = Loan "Test 1" 10.0 10.0 5000
                    loans = [loan]
                    paymentStrategy = Avalanche
                    paymentPlan = toPaymentPlan yearsToPayoff loans 
                    model = Model loans emptyLoan [] yearsToPayoff paymentStrategy 500.0 (Just paymentPlan)
                in
                generatePaymentPlan model 
                    |> Tuple.first 
                    |> (\x -> x.paymentPlan)
                    |> Maybe.map List.length
                    |> Maybe.withDefault 0
                    |> Expect.greaterThan 1
        ]
