module LoanTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Loan exposing (..)

suite : Test
suite = 
    describe "The Loan module"
        [ test "getMinimumPaymentAmount" <|
            \_ -> 
                let
                    principal = 10000.0
                    apr = 20.0
                    aprAsPercentage = apr / 100.0
                    payPeriodsPerYear = 12.0
                    r = aprAsPercentage / payPeriodsPerYear
                    yearsToPayoff = 20
                    totalNumberOfPaymentPeriods = payPeriodsPerYear * (toFloat yearsToPayoff)
                    expectedValue = (r * principal) / (1 - (1 + r) ^ -totalNumberOfPaymentPeriods)
                in
                getMinimumPaymentAmount principal apr yearsToPayoff
                    |> Expect.within (Absolute 0.0001) expectedValue
        , describe "toPaymentPlan"
            [ test "minimum is sufficient for payment period" <|
                \_ -> 
                    let
                        minimumPayment = 20.0
                        loan = Loan "Test Loan" 1.0 minimumPayment 200.0
                        yearsToPayoff = 10
                        expectedPaymentSequence = [PaymentSequence loan minimumPayment [] False]
                    in
                    toPaymentPlan yearsToPayoff [loan] 
                        |> Expect.equal expectedPaymentSequence
            , test "minimum is not sufficient for payment period" <|
                \_ -> 
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        loan = Loan "Test Loan" apr minimumPayment principal
                        expectedPaymentSequence = [PaymentSequence loan actualMinimumPayment [] False]
                    in
                    toPaymentPlan yearsToPayoff [loan] 
                        |> Expect.equal expectedPaymentSequence
            ]
        ]