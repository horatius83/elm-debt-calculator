module LoanTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Loan exposing (getMinimumPaymentAmount)

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
        ]