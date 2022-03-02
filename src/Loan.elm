module Loan exposing (..)


type alias Loan =
    { name : String
    , apr : Float
    , minimum : Float
    , principal : Float
    }


type alias Payment =
    Float


type alias PaymentSequence =
    { loan : Loan
    , actualMinimum : Float
    , payments : List Payment
    , isPaidOff : Bool
    }


type alias PaymentPlan =
    List PaymentSequence


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
    in
    List.map (\ln -> PaymentSequence ln (getActualMinimum ln) [] False) loans


getMinimumTotalAmount : PaymentPlan -> Float
getMinimumTotalAmount =
    List.foldl (\ps totalAmount -> totalAmount + ps.actualMinimum) 0


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
        ( bonus, newPaymentSequence ++ [ PaymentSequence loan actualMinimum payments True ] )

    else if principalRemaining < minimumAndBonus then
        ( minimumAndBonus - principalRemaining, newPaymentSequence ++ [ PaymentSequence loan actualMinimum (payments ++ [ principalRemaining ]) True ] )

    else
        ( 0, newPaymentSequence ++ [ PaymentSequence loan actualMinimum (payments ++ [ minimumAndBonus ]) False ] )


strategy : (PaymentSequence -> comparable) -> PaymentPlan -> Float -> PaymentPlanResult
strategy sortFunction paymentPlan maximumAmount =
    let
        sortedPaymentPlan =
            List.sortBy sortFunction paymentPlan

        minimumTotalPayment =
            getMinimumTotalAmount paymentPlan

        bonusAmount =
            maximumAmount - minimumTotalPayment

        ( _, newPaymentPlan ) =
            List.foldl calculateNewPayment ( bonusAmount, [] ) sortedPaymentPlan

        areThereAnyFurtherPayments =
            List.any (\ps -> not ps.isPaidOff) newPaymentPlan
    in
    if minimumTotalPayment < maximumAmount then
        MaximumTotalAmountTooLow minimumTotalPayment

    else if areThereAnyFurtherPayments then
        NoFurtherPaymentsToBeMade newPaymentPlan

    else
        PaymentsRemaining newPaymentPlan


avalanche : PaymentPlan -> Float -> PaymentPlanResult
avalanche =
    strategy (\paymentSequence -> paymentSequence.loan.apr)


snowball : PaymentPlan -> Float -> PaymentPlanResult
snowball =
    strategy (\paymentSequence -> paymentSequence.loan.principal)



-- Strategies
--  Avalanche
--  Snowball
--  Double Double - pick payments to double, triple, etc. in a given month and apply that
--  Savvy Investor - pick an interest rate, anything under that, pay the minimum and invest the rest in ETFs
--  Emergency Fund - prioritize saving into an emergency fund before paying off debt
