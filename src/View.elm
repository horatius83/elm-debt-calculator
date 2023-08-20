module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, disabled, href, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import State exposing (EmergencyFundPlan, FormState(..), Loan, Model, Msg(..), PaymentSequence, PaymentStrategy(..))
import Time exposing (Month(..))
import TimeUtil exposing (getNextMonth, getNextYear, monthToString)


viewTextInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewTextInput labelText val id callback =
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , br [] []
        , input
            [ value val
            , onInput callback
            , attribute "name" id
            , attribute "type" "text"
            ]
            []
        ]


viewCheckboxInput : String -> String -> Bool -> Msg -> Html Msg
viewCheckboxInput labelText id val callback =
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , input
            [ attribute "type" "checkbox"
            , checked val
            , onClick callback
            , attribute "name" id
            ]
            []
        ]


viewLoan : Int -> Loan -> Html Msg
viewLoan index loan =
    let
        toCash value =
            String.fromFloat value
                |> (\v -> "$" ++ v)

        toPercent value =
            String.fromFloat value
                |> (\v -> v ++ "%")
    in
    tr []
        [ td [] [ text loan.name ]
        , td [ class "align-right" ] [ text <| toCash loan.principal ]
        , td [ class "align-right" ] [ text <| toCash loan.minimum ]
        , td [ class "align-right" ] [ text <| toPercent loan.apr ]
        , td [] [ button [ onClick (DeleteLoan index) ] [ text "Delete" ] ]
        ]


viewSelect : String -> String -> List String -> (String -> Msg) -> Html Msg
viewSelect labelText id options stringToMsg =
    let
        viewOption txt =
            option [ attribute "value" txt ] [ text txt ]

        optionsAsHtml =
            List.map viewOption options
    in
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , br [] []
        , select
            [ attribute "name" id
            , attribute "id" id
            , onInput stringToMsg
            ]
            optionsAsHtml
        ]


viewPaymentPlan : Int -> Month -> List PaymentSequence -> Html Msg
viewPaymentPlan currentYear currentMonth paymentPlan =
    let
        payments =
            viewPaymentSequence currentYear currentMonth paymentPlan
    in
    div [] payments


viewPaymentSequence : Int -> Month -> List PaymentSequence -> List (Html Msg)
viewPaymentSequence currentYear currentMonth paymentSequence =
    let
        viewDate year month =
            let
                yearAsText =
                    String.fromInt year

                monthAsText =
                    monthToString month
            in
            monthAsText ++ " " ++ yearAsText |> text

        getThisMonthsPaymentsAcc ps acc =
            case ps.payments of
                [] ->
                    acc

                x :: _ ->
                    List.append acc [ ( ps.loan.name, x ) ]

        thisMonthsPayments =
            List.foldl getThisMonthsPaymentsAcc [] paymentSequence

        nextMonthsPaymentSequenceAcc ps acc =
            case ps.payments of
                [] ->
                    acc

                _ :: xs ->
                    List.append acc [ PaymentSequence ps.loan ps.actualMinimum xs ps.isPaidOff ]

        nextMonthsPaymentSequence =
            List.foldl nextMonthsPaymentSequenceAcc [] paymentSequence

        viewMonthlyPayment loanName amount =
            tr []
                [ td [] [ text loanName ]
                , td [] [ viewMoney amount ]
                ]

        thisSection =
            section []
                [ div [] [ h2 [] [ viewDate currentYear currentMonth ] ]
                , table [] (List.map (\( name, amount ) -> viewMonthlyPayment name amount) thisMonthsPayments)
                ]
    in
    case thisMonthsPayments of
        [] ->
            []

        _ ->
            thisSection :: viewPaymentSequence (getNextYear currentYear currentMonth) (getNextMonth currentMonth) nextMonthsPaymentSequence


viewMoney : Float -> Html Msg
viewMoney f =
    (f * 100.0)
        |> round
        |> toFloat
        |> (\x -> x / 100.0)
        |> String.fromFloat
        |> (\x -> "$" ++ x)
        |> text


viewPaymentStrategy : Model -> Html Msg
viewPaymentStrategy model =
    let
        isCalculatePaymentPlanButtonDisabled =
            let
                hasLoans =
                    List.length model.loans > 0

                maxNumberOfYears =
                    String.toFloat model.strategyForm.maxNumberOfYears

                maxTotalPayment =
                    String.toFloat model.strategyForm.maxTotalPayment
            in
            case ( hasLoans, maxNumberOfYears, maxTotalPayment ) of
                ( True, Just _, Just _ ) ->
                    False

                _ ->
                    True

        paymentStrategyOptions =
            [ "Highest Interest First"
            , "Lowest Principal First"
            ]

        optionToStrategy option =
            case option of
                "Lowest Principal First" ->
                    ChoosePaymentStrategy Snowball

                _ ->
                    ChoosePaymentStrategy Avalanche

        displayEmergencyFundOptions =
            case model.strategyForm.emergencyFund of
                Just _ ->
                    True

                Nothing ->
                    False

        strategyFields =
            [ viewTextInput "Maximum number of years to payoff" model.strategyForm.maxNumberOfYears "years-to-payoff" UpdateYearsToPayoff
            , viewTextInput "Maximum total monthly payment" model.strategyForm.maxTotalPayment "total-minimum-amount" UpdateMaximumTotalPayment
            , viewSelect "Payment Strategy" "payment-strategy" paymentStrategyOptions optionToStrategy
            , viewCheckboxInput "Build Emergency Fund?" "has-emergency-fund" displayEmergencyFundOptions ToggleHasEmergencyFund
            ]

        emergencyFundFields =
            case model.strategyForm.emergencyFund of
                Just ef ->
                    [ viewTextInput "Maximum Emergency Fund Amount" ef.maxAmountAsString "max-emergency-fund-amount" UpdateEmergencyFundMaxAmount
                    , viewTextInput "Percentage of Bonus Funds to Apply" ef.percentageToApplyAsString "percentage-to-apply" UpdateEmergencyFundPercentage
                    ]

                Nothing ->
                    []

        buttons =
            [ button
                [ disabled isCalculatePaymentPlanButtonDisabled
                , onClick GeneratePaymentPlan
                ]
                [ text "Show Payment Plan" ]
            , button
                [ disabled isCalculatePaymentPlanButtonDisabled
                , onClick GeneratePaymentPlanAsPdf
                ]
                [ text "Show Payment Plan PDF" ]
            ]
    in
    form [ onSubmit DoNothing ]
        [ fieldset []
            (strategyFields ++ emergencyFundFields ++ buttons)
        ]


viewNewLoan : Model -> Html Msg
viewNewLoan model =
    let
        canPickPaymentStrategy =
            case model.loans of
                [] ->
                    True

                _ ->
                    False

        loan =
            model.newLoanForm

        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False

        cannotAddNewLoan =
            [ loan.principal
            , loan.minimum
            , loan.apr
            ]
                |> List.map String.toFloat
                |> List.all isJust
                |> not
    in
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewTextInput "Name" loan.name "new-loan-name" UpdateLoanName
            , viewTextInput "Principal" loan.principal "new-loan-principal" UpdateLoanPrincipal
            , viewTextInput "Minimum" loan.minimum "new-loan-minimum" UpdateLoanMinimum
            , viewTextInput "APR" loan.apr "new-loan-apr" UpdateLoanApr
            , button
                [ onClick AddLoan
                , disabled cannotAddNewLoan
                ]
                [ text "Add Loan" ]
            , button [] [ text "Load from File" ]
            , button [] [ text "Save to File" ]
            , button [ onClick ResetNewLoan ] [ text "Reset" ]
            , button
                [ disabled canPickPaymentStrategy
                , onClick (ChangeFormState EnterPaymentStrategy)
                ]
                [ text "Next" ]
            ]
        ]
