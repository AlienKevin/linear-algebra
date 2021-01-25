module Row exposing (Row, subtract, scale, getPivotIndex, getEntry)

import Array exposing (Array)
import Entry exposing (Entry(..))
import Array.Extra


type alias Row =
    Array Entry


scale : Entry -> Row -> Row
scale entry row =
    Array.map (Entry.multiply entry) row


subtract : Row -> Row -> Row
subtract =
    binaryOperation Entry.subtract


binaryOperation : (Entry -> Entry -> Entry) -> Row -> Row -> Row
binaryOperation operation =
    Array.Extra.map2
        operation


getPivotIndex : Row -> Maybe Int
getPivotIndex row =
    let
        helper i =
            if i >= Array.length row then
                Nothing
            else if not <| Entry.equal (getEntry i row) Entry.zero then
                Just i
            else
                helper (i + 1)
    in
    helper 0


getEntry : Int -> Row -> Entry
getEntry index row =
    Maybe.withDefault Entry.zero <| Array.get index row
