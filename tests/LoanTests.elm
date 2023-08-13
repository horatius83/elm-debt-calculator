module LoanTests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (..)

import Loan exposing (..)
import State exposing (Msg(..), Loan, PaymentSequence, PaymentPlan, EmergencyFundPlan, EmergencyFundPayments)

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
                        |> (\x -> x.payments)
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
                        |> (\x -> x.payments)
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
                    in
                        calculateNewPayment paymentSequence (bonus, emptyPaymentPlan)
                        |> (\x -> (Tuple.first x, (Tuple.second x).payments))
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
                        newPaymentSequence = [ PaymentSequence loan minimumPayment [ principal ] True ]
                    in
                        calculateNewPayment paymentSequence (bonus, emptyPaymentPlan)
                        |> (\x -> (Tuple.first x, (Tuple.second x).payments))
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
                        calculateNewPayment paymentSequence (bonus, emptyPaymentPlan)
                        |> (\x -> (Tuple.first x, (Tuple.second x).payments))
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
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] Nothing
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
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] Nothing
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
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] True] Nothing
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
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] Nothing
                        totalAmount = actualMinimumPayment + 10.0
                        isPaymentsRemaining x = case x of
                            PaymentsRemaining _ -> True
                            _ -> False
                    in
                        avalanche paymentPlan totalAmount
                        |> isPaymentsRemaining
                        |> Expect.equal True
            ]
            , describe "Emergency Fund"
            [ test "Should divide bonus" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        emergencyFundPlan = EmergencyFundPlan 5000 "" 0.5 ""
                        emergencyFund = Just <| EmergencyFundPayments emergencyFundPlan []
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] emergencyFund
                        totalAmount = actualMinimumPayment + 10.0
                        amountPaidToEmergencyFund x = case x of 
                            PaymentsRemaining r -> 
                                case r.savings of
                                    Just ss -> 
                                        case ss.payments of
                                            [p] -> Just p
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing

                        amountPaidToLoans x = case x of
                            PaymentsRemaining r ->
                                case r.payments of
                                    [p] -> case p.payments of
                                        [pp] -> Just pp
                                        _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                    in
                        avalanche paymentPlan totalAmount
                        |> (\pp -> (amountPaidToEmergencyFund pp, amountPaidToLoans pp))
                        |> Expect.equal (Just 5.0, Just <| actualMinimumPayment + 5.0)
            , test "Should add emergency fund bonuses correctly" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        emergencyFundPlan = EmergencyFundPlan 5000 "" 0.5 ""
                        emergencyFund = Just <| EmergencyFundPayments emergencyFundPlan []
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] emergencyFund
                        totalAmount = actualMinimumPayment + 10.0

                        amountPaidToEmergencyFund x = case x of 
                            PaymentsRemaining r -> 
                                case r.savings of
                                    Just ss -> 
                                        case ss.payments of
                                            [_, p] -> Just p
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing

                        amountPaidToLoans x = case x of
                            PaymentsRemaining r ->
                                case r.payments of
                                    [p] -> case p.payments of
                                        [_, pp] -> Just pp
                                        _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                        firstRun = case avalanche paymentPlan totalAmount of
                            PaymentsRemaining r -> Just r
                            _ -> Nothing
                    in
                        Maybe.map (\p -> avalanche p totalAmount) firstRun
                        |> (\pp -> Maybe.map (\ppp -> (amountPaidToEmergencyFund ppp, amountPaidToLoans ppp)) pp)
                        |> Expect.equal (Just (Just 5.0, Just (actualMinimumPayment + 5.0)))
            , test "Should stop adding to emergency fund when fully funded" <|
                \_ ->
                    let
                        minimumPayment = 20.0
                        principal = 2000.0
                        yearsToPayoff = 10
                        apr = 20.0
                        loan = Loan "Test Loan" apr minimumPayment principal
                        actualMinimumPayment = getMinimumPaymentAmount principal apr yearsToPayoff
                        emergencyFundPlan = EmergencyFundPlan 5 "" 0.5 ""
                        emergencyFund = Just <| EmergencyFundPayments emergencyFundPlan []
                        paymentPlan = PaymentPlan [PaymentSequence loan actualMinimumPayment [] False] emergencyFund
                        totalAmount = actualMinimumPayment + 10.0

                        amountPaidToEmergencyFund x = case x of 
                            PaymentsRemaining r -> 
                                case r.savings of
                                    Just ss -> 
                                        case ss.payments of
                                            [_, p] -> Just p
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing

                        amountPaidToLoans x = case x of
                            PaymentsRemaining r ->
                                case r.payments of
                                    [p] -> case p.payments of
                                        [_, pp] -> Just pp
                                        _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                        firstRun = case avalanche paymentPlan totalAmount of
                            PaymentsRemaining r -> Just r
                            _ -> Nothing
                    in
                        Maybe.map (\p -> avalanche p totalAmount) firstRun
                        |> (\pp -> Maybe.map (\ppp -> (amountPaidToEmergencyFund ppp, amountPaidToLoans ppp)) pp)
                        |> Expect.equal (Just (Just 0, Just (actualMinimumPayment + 10.0)))
            ]
            , describe "getMinimumTotalAmount"
            [ test "Gets total amount" <|
                \_ -> 
                    let
                        loans = 
                            [ Loan "Test Loan 1" 10.0 50.0 5000.0
                            , Loan "Test Loan 2" 20.0 20.0 2000.0]
                        payments = List.map (\{name, apr, minimum, principal} -> 
                            PaymentSequence (Loan name apr minimum principal) minimum [] False) loans
                        paymentPlan = PaymentPlan payments Nothing
                    in
                    getMinimumTotalAmount paymentPlan
                    |> Expect.within (Absolute 0.001)  70.0
            , test "Does not add paid off loans" <|
                \_ ->
                    let
                        loans = 
                            [ (Loan "Test Loan 1" 10.0 50.0 5000.0, False)
                            , (Loan "Test Loan 2" 20.0 20.0 2000.0, True)]
                        payments = List.map (\({name, apr, minimum, principal}, isPaidOff) -> 
                            PaymentSequence (Loan name apr minimum principal) minimum [] isPaidOff) loans
                        paymentPlan = PaymentPlan payments Nothing
                    in
                    getMinimumTotalAmount paymentPlan
                    |> Expect.within (Absolute 0.001)  50.0
            ]
        ]