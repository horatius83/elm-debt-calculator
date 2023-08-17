module Loan exposing (..)

import State exposing (Loan, Msg(..), PaymentPlan, PaymentSequence)


addPaymentToPaymentPlan : PaymentPlan -> PaymentSequence -> PaymentPlan
addPaymentToPaymentPlan pp ps =
    let
        oldPayments =
            pp.payments

        newPayments =
            oldPayments ++ [ ps ]
    in
    { pp | payments = newPayments }


emptyPaymentPlan : PaymentPlan
emptyPaymentPlan =
    PaymentPlan [] Nothing


type PaymentPlanResult
    = MaximumTotalAmountTooLow Float
    | NoFurtherPaymentsToBeMade PaymentPlan
    | PaymentsRemaining PaymentPlan


getMinimumPaymentAmount : Float -> Float -> Int -> Float
getMinimumPaymentAmount principal apr maxNumberOfYears =
    let
        ratePerPeriod =
            apr / (12.0 * 100.0)

        -- percent (p) to decimal = p / 100, divide by number of periods (12)
        maxNumberOfPayments =
            maxNumberOfYears
                * 12
                |> toFloat

        numerator =
            ratePerPeriod * principal

        denominator =
            1 - (1 + ratePerPeriod) ^ (-1 * maxNumberOfPayments)
    in
    numerator / denominator


getMinimumPaymentAmountForLoan : Loan -> Int -> Float
getMinimumPaymentAmountForLoan loan maxNumberOfYears =
    getMinimumPaymentAmount loan.principal loan.apr maxNumberOfYears


toPaymentPlan : Int -> List Loan -> PaymentPlan
toPaymentPlan maxNumberOfYears loans =
    let
        calculatedMinimum loan =
            getMinimumPaymentAmount loan.principal loan.apr maxNumberOfYears

        getActualMinimum loan =
            let
                cm =
                    calculatedMinimum loan
            in
            if cm < loan.minimum then
                loan.minimum

            else
                cm

        payments =
            List.map (\ln -> PaymentSequence ln (getActualMinimum ln) [] False) loans
    in
    PaymentPlan payments Nothing


getMinimumTotalAmount : PaymentPlan -> Float
getMinimumTotalAmount pp =
    let
        payments =
            pp.payments

        f ps totalAmount =
            if ps.isPaidOff then
                totalAmount

            else
                totalAmount + ps.actualMinimum
    in
    List.foldl f 0 payments


calculateNewPayment : PaymentSequence -> ( Float, PaymentPlan ) -> ( Float, PaymentPlan )
calculateNewPayment { loan, actualMinimum, payments, isPaidOff } ( bonus, newPaymentSequence ) =
    let
        totalAmountPaid =
            List.foldl (\payment a -> a + payment) 0 payments

        principalRemaining =
            loan.principal - totalAmountPaid

        minimumAndBonus =
            actualMinimum + bonus
    in
    if isPaidOff then
        ( bonus, addPaymentToPaymentPlan newPaymentSequence (PaymentSequence loan actualMinimum payments True) )

    else if principalRemaining < minimumAndBonus then
        ( minimumAndBonus - principalRemaining, addPaymentToPaymentPlan newPaymentSequence (PaymentSequence loan actualMinimum (payments ++ [ principalRemaining ]) True) )

    else
        ( 0, addPaymentToPaymentPlan newPaymentSequence (PaymentSequence loan actualMinimum (payments ++ [ minimumAndBonus ]) False) )


strategy : (PaymentSequence -> comparable) -> PaymentPlan -> Float -> PaymentPlanResult
strategy sortFunction paymentPlan maximumAmount =
    let
        sortedPaymentPlan =
            List.sortBy sortFunction paymentPlan.payments

        minimumTotalPayment =
            getMinimumTotalAmount paymentPlan

        inv x =
            1.0 - x

        bonusAmount =
            case paymentPlan.savings of
                Just efp ->
                    let
                        totalPaidIntoEmergencyFund =
                            List.foldl (\acc x -> acc + x) 0 efp.payments
                    in
                    if totalPaidIntoEmergencyFund < efp.plan.maxAmount then
                        (maximumAmount - minimumTotalPayment) * inv efp.plan.percentageToApply

                    else
                        maximumAmount - minimumTotalPayment

                Nothing ->
                    maximumAmount - minimumTotalPayment

        emptyPp =
            case paymentPlan.savings of
                Just efp ->
                    let
                        savingsAmount =
                            let
                                totalPaidIntoEmergencyFund =
                                    List.foldl (\acc x -> acc + x) 0 efp.payments
                            in
                            if totalPaidIntoEmergencyFund < efp.plan.maxAmount then
                                (maximumAmount - minimumTotalPayment) * inv efp.plan.percentageToApply

                            else
                                0

                        newEfpPlans =
                            { efp | payments = efp.payments ++ [ savingsAmount ] }
                    in
                    PaymentPlan [] <| Just newEfpPlans

                Nothing ->
                    PaymentPlan [] Nothing

        ( _, newPaymentPlan ) =
            List.foldl calculateNewPayment ( bonusAmount, emptyPp ) sortedPaymentPlan

        areThereAnyFurtherPayments =
            List.any (\ps -> not ps.isPaidOff) newPaymentPlan.payments
    in
    if minimumTotalPayment > maximumAmount then
        MaximumTotalAmountTooLow minimumTotalPayment

    else if not areThereAnyFurtherPayments then
        NoFurtherPaymentsToBeMade newPaymentPlan

    else
        PaymentsRemaining newPaymentPlan


avalanche : PaymentPlan -> Float -> PaymentPlanResult
avalanche =
    strategy (\paymentSequence -> -1.0 * paymentSequence.loan.apr)


snowball : PaymentPlan -> Float -> PaymentPlanResult
snowball =
    strategy (\paymentSequence -> -1.0 * paymentSequence.loan.principal)



-- Strategies
--  Double Double - pick payments to double, triple, etc. in a given month and apply that
--  Savvy Investor - pick an interest rate, anything under that, pay the minimum and invest the rest in ETFs
--  Emergency Fund - prioritize saving into an emergency fund before paying off debt
