module Main exposing (..)

import Dict
import Html exposing (Html, button, div, h1, h3, h4, text)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick)
import Round
import Time exposing (Time, second)


---- MODEL ----
type alias Config =
    { quantity : Float, cost : Float, productivity : Float, coefficient : Float }

baseConfig : Dict.Dict String Config
baseConfig =
    Dict.fromList
        [ ( "Lemonade Stand", { quantity = 1, cost = 3.7, productivity = 1.67, coefficient = 1.07 } )
        , ( "Newspaper Delivery", { quantity = 0, cost = 60, productivity = 20, coefficient = 1.15 } )
        , ( "Car Wash", { quantity = 0, cost = 720, productivity = 90, coefficient = 1.14 } )
        , ( "Pizza Delivery", { quantity = 0, cost = 8640, productivity = 360, coefficient = 1.13 } )
        , ( "Donut Shop", { quantity = 0, cost = 103680, productivity = 2160, coefficient = 1.12 } )
        , ( "Shrimp Boat", { quantity = 0, cost = 1244160, productivity = 6480, coefficient = 1.11 } )
        , ( "Hockey Team", { quantity = 0, cost = 14929920, productivity = 19440, coefficient = 1.1 } )
        , ( "Movie Studio", { quantity = 0, cost = 179159040, productivity = 58320, coefficient = 1.09 } )
        , ( "Bank", { quantity = 0, cost = 2149908480, productivity = 174960, coefficient = 1.08 } )
        , ( "Oil Company", { quantity = 0, cost = 25798901760, productivity = 804816, coefficient = 1.07 } )
        ]

type alias Model =
    { money : Float, config : Dict.Dict String Config }

init : ( Model, Cmd Msg )
init =
    ( { money = 1, config = baseConfig }, Cmd.none )

---- UPDATE ----
type Msg
    = Purchase String
    | Tick Time

getProfit model =
    Dict.foldl (\key x acc -> acc + x.quantity * x.productivity) 0 model.config

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Purchase business ->
            ( { model
                | money = model.money - getCost model business
                , config =
                    Dict.update business
                        (\x ->
                            case x of
                                Just x ->
                                    Just { x | quantity = x.quantity + 1, cost = x.cost * x.coefficient }

                                Nothing ->
                                    Nothing
                        )
                        model.config
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | money = model.money + getProfit model }, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

---- VIEW ----
getCost model business =
    case Dict.get business model.config of
        Just x ->
            x.cost

        Nothing ->
            0

getProductivity model business =
    case Dict.get business model.config of
        Just x ->
            x.productivity

        Nothing ->
            0

getMultiplier model business =
    case Dict.get business model.config of
        Just x ->
            x.quantity

        Nothing ->
            0

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("£" ++ Round.round 2 model.money) ]
        , businessDetails model "Lemonade Stand"
        , businessDetails model "Newspaper Delivery"
        , businessDetails model "Car Wash"
        , businessDetails model "Pizza Delivery"
        , businessDetails model "Donut Shop"
        , businessDetails model "Shrimp Boat"
        , businessDetails model "Hockey Team"
        , businessDetails model "Movie Studio"
        , businessDetails model "Bank"
        , businessDetails model "Oil Company"
        ]

businessDetails model business =
    div []
        [ h3 [] [ text business ]
        , h4 [] [ text ("Cost: £" ++ (getCost model business |> Round.round 2)) ]
        , h4 [] [ text ("Productivity: £" ++ toString (getProductivity model business) ++ "/s " ++ "per " ++ business) ]
        , h4 [] [ text ("Quantity: " ++ toString (getMultiplier model business)) ]
        , button [ onClick (Purchase business), disabled (model.money < getCost model business) ] [ text ("Buy " ++ business) ]
        ]

---- PROGRAM ----
main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
