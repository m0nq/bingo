module Bingo exposing (..)

import Html


-- main =
--     Html.text (String.repeat 3 (String.toUpper "Mike's Game #3"))


main =
    "Mike's Game #3"
        |> String.toUpper
        |> Html.text
