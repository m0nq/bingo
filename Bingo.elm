module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


playerInfo : String -> a -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


playerInfoText : String -> a -> Html msg
playerInfoText name gameNumber =
    playerInfo name gameNumber
        |> String.toUpper
        |> text


main : Html msg
main =
    h2 [ id "info", class "classy" ] [ playerInfoText "Mike" 3 ]
