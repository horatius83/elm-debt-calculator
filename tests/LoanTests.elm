module LoanTests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
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
        , describe "calculateNewPayment" 
            [ test "isPaidOff" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentSequence = PaymentSequence loan actualMinimumPayment [] True
                        bonus = 100
                        newPaymentSequence = []
                    in
                        calculateNewPayment paymentSequence (bonus, newPaymentSequence)
                        |> Expect.equal (bonus, [paymentSequence])
            , test "principal remaining less than minimum" <|
                \_ -> 
                    let
                        minimumPayment = 20.0
                        apr = 20.0
                        principal = minimumPayment - 1.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        paymentSequence = PaymentSequence loan minimumPayment [] False
                        bonus = 100
                        emptyPaymentSequence = []
                        newPaymentSequence = [ PaymentSequence loan minimumPayment [ principal ] True ]
                    in
                        calculateNewPayment paymentSequence (bonus, emptyPaymentSequence)
                        |> Expect.equal (bonus + (minimumPayment - principal), newPaymentSequence)
            , test "principal remaining greater than minimum" <|
                \_ -> 
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentSequence = PaymentSequence loan actualMinimumPayment [] False
                        bonus = 100
                        newPaymentSequence = [ PaymentSequence loan actualMinimumPayment [ actualMinimumPayment + bonus ] False ]
                    in
                        calculateNewPayment paymentSequence (bonus, [])
                        |> Expect.equal (0, newPaymentSequence)
            ]
        , describe "avalanche"
            [ test "MaximumTotalAmountTooLow" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentPlan = [PaymentSequence loan actualMinimumPayment [] False]
                        totalAmount = actualMinimumPayment - 10.0
                        isMaximumTotalAmountTooLow x = case x of
                            MaximumTotalAmountTooLow _ -> True
                            _ -> False
                    in
                        avalanche paymentPlan totalAmount
                        |> isMaximumTotalAmountTooLow
                        |> Expect.equal True
            , test "NoFurtherPaymentsToBeMade bonus left over" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentPlan = [PaymentSequence loan actualMinimumPayment [] False]
                        totalAmount = principal + 10.0
                        isNoFurtherPaymentsToBeMade x = case x of
                            NoFurtherPaymentsToBeMade _ -> True
                            _ -> False
                    in
                        avalanche paymentPlan totalAmount
                        |> isNoFurtherPaymentsToBeMade
                        |> Expect.equal True
            , test "NoFurtherPaymentsToBeMade isPaidOff flag" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentPlan = [PaymentSequence loan actualMinimumPayment [] True]
                        totalAmount = actualMinimumPayment
                        isNoFurtherPaymentsToBeMade x = case x of
                            NoFurtherPaymentsToBeMade _ -> True
                            _ -> False
                    in
                        avalanche paymentPlan totalAmount
                        |> isNoFurtherPaymentsToBeMade
                        |> Expect.equal True
            , test "PaymentsRemaining" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        paymentPlan = [PaymentSequence loan actualMinimumPayment [] False]
                        totalAmount = actualMinimumPayment + 10.0
                        isPaymentsRemaining x = case x of
                            PaymentsRemaining _ -> True
                            _ -> False
                    in
                        avalanche paymentPlan totalAmount
                        |> isPaymentsRemaining
                        |> Expect.equal True
            ]
        ]