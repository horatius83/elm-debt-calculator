module Util exposing (..)


maybesToList : List (Maybe a) -> List a
maybesToList list =
    case list of
        (Just x) :: xs ->
            x :: maybesToList xs

        Nothing :: xs ->
            maybesToList xs

        [] ->
            []
