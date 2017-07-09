module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


playerInfoText name gameNumber =
    playerInfo name gameNumber
        |> String.toUpper
        |> text


main =
    h2 [ id "info", class "classy" ] [ playerInfoText "Mike" 3 ]
