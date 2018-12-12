module Day10 exposing (main)

import Array
import Browser
import Browser.Events
import Data.Day10 exposing (inputStr)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , keyword
        , loop
        , spaces
        , succeed
        , symbol
        )



-- Section: inputParsing


initialLights : List Light
initialLights =
    Parser.run inputParser inputStr
        |> Result.withDefault []


inputParser : Parser (List Light)
inputParser =
    loop [] lightCollector


lightCollector : List Light -> Parser (Step (List Light) (List Light))
lightCollector collected =
    Parser.oneOf
        [ succeed (\light -> Loop (light :: collected))
            |= lightParser
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse collected))
        ]


type alias Light =
    { position : { x : Int, y : Int }
    , velocity : { x : Int, y : Int }
    }


makeXY : Int -> Int -> { x : Int, y : Int }
makeXY x y =
    { x = x, y = y }


lightParser : Parser Light
lightParser =
    succeed Light
        |. spaces
        |= positionParser
        |= velocityParser
        |. spaces


positionParser : Parser { x : Int, y : Int }
positionParser =
    succeed identity
        |. spaces
        |. keyword "position="
        |= pairParser
        |. spaces


velocityParser : Parser { x : Int, y : Int }
velocityParser =
    succeed identity
        |. spaces
        |. keyword "velocity="
        |= pairParser
        |. spaces


pairParser : Parser { x : Int, y : Int }
pairParser =
    succeed makeXY
        |. symbol "<"
        |. spaces
        |= int
        |. symbol ","
        |. spaces
        |= int
        |. symbol ">"


int : Parser Int
int =
    Parser.oneOf
        [ succeed negate
            |. symbol "-"
            |= Parser.int
        , Parser.int
        ]



-- Section: Part One


type alias Model =
    { bounds : Bounds
    , lights : List Light
    , speeds : List Int
    , animate : Bool
    , time : Int
    }


type alias Bounds =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


initialModel : Model
initialModel =
    { lights = initialLights
    , bounds = getBounds initialLights
    , speeds = [ 100, -10, 1 ]
    , animate = True
    , time = 0
    }


boundArea : Bounds -> Int
boundArea bounds =
    (bounds.bottom - bounds.top) * (bounds.right - bounds.left)



-- Update


update : Float -> Model -> ( Model, Cmd Float )
update deltaMs model =
    case model.speeds of
        deltaS :: nextSpeeds ->
            let
                newLights =
                    updateLights deltaS model.lights

                newBounds =
                    getBounds newLights

                boundChange =
                    boundArea newBounds - boundArea model.bounds

                newSpeeds =
                    if boundChange > 0 then
                        nextSpeeds

                    else
                        model.speeds
            in
            if boundChange > 0 && deltaS == 1 then
                ( { model | animate = False }, Cmd.none )

            else
                ( { lights = newLights
                  , bounds = newBounds
                  , animate = True
                  , speeds = newSpeeds
                  , time = model.time + deltaS
                  }
                , Cmd.none
                )

        [] ->
            ( { model | animate = False }, Cmd.none )


updateLights : Int -> List Light -> List Light
updateLights secs lights =
    List.map (updateLight secs) lights


updateLight : Int -> Light -> Light
updateLight secs light =
    { light
        | position =
            { x = light.position.x + (secs * light.velocity.x)
            , y = light.position.y + (secs * light.velocity.y)
            }
    }


getBounds : List Light -> Bounds
getBounds lights =
    case lights of
        first :: others ->
            List.foldl
                (\light bounds ->
                    { left = min light.position.x bounds.left
                    , right = max light.position.x bounds.right
                    , top = min light.position.y bounds.top
                    , bottom = max light.position.y bounds.bottom
                    }
                )
                (Bounds first.position.x
                    first.position.x
                    first.position.y
                    first.position.y
                )
                others

        [] ->
            Bounds 0 0 0 0



-- View


view : Model -> Browser.Document msg
view model =
    { title = "Sim: Day 10"
    , body =
        [ viewLights model.bounds model.lights
        , viewTime model
        ]
    }


viewLights : Bounds -> List Light -> Html msg
viewLights bounds lights =
    div
        [ style "position" "relative"
        , style "margin" "auto"
        , style "width" "100vh"
        , style "height" "100vh"
        , style "background-color" "black"
        ]
        (List.map (lightView bounds) lights)


lightView : Bounds -> Light -> Html msg
lightView bounds light =
    let
        ( xPer, yPer ) =
            toPercentCoords bounds light
    in
    div
        [ class "light"
        , style "left" (String.fromFloat xPer ++ "%")
        , style "top" (String.fromFloat yPer ++ "%")
        ]
        []


toPercentCoords : Bounds -> Light -> ( Float, Float )
toPercentCoords bounds light =
    let
        xPer =
            (toFloat (light.position.x - bounds.left) / toFloat (bounds.right - bounds.left)) * 100

        yPer =
            (toFloat (light.position.y - bounds.top) / toFloat (bounds.right - bounds.left)) * 100
    in
    ( xPer, yPer )


viewTime : Model -> Html msg
viewTime model =
    if not model.animate then
        div [ class "time" ]
            [ text ("Simulated " ++ String.fromInt model.time ++ "s")
            ]

    else
        text ""


main : Program () Model Float
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \model ->
                if model.animate then
                    Browser.Events.onAnimationFrameDelta identity

                else
                    Sub.none
        }



-- Section: Part Two


part2 =
    "See Part One"
