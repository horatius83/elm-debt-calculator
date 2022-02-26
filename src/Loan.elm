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
    }


type alias PaymentPlan =
    List PaymentSequence


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
    List.map (\ln -> PaymentSequence ln (getActualMinimum ln) []) loans


createAnotherMonthlyPaymentPlan : List PaymentSequence -> PaymentPlan
createAnotherMonthlyPaymentPlan paymentPlan =
    let
        createNewPayment loan minimumPayment payments =
            let
                totalAmountPaid =
                    List.foldl (\payment a -> a + payment) 0 payments

                principalRemaining =
                    loan.principal - totalAmountPaid
            in
            if principalRemaining < minimumPayment then
                principalRemaining

            else
                minimumPayment
    in
    List.map (\{ loan, actualMinimum, payments } -> PaymentSequence loan actualMinimum (payments ++ [ createNewPayment loan actualMinimum payments ])) paymentPlan


getMinimumTotalAmount : PaymentPlan -> Float
getMinimumTotalAmount =
    List.foldl (\ps totalAmount -> totalAmount + ps.actualMinimum) 0



-- Strategies
--  Avalanche
--  Snowball
--  Double Double
