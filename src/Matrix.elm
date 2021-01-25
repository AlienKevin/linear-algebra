module Matrix exposing (Matrix, fromEntryLists, fromNumberLists, toRref, toString)

import Array exposing (Array)
import Array.Extra
import Entry exposing (Entry(..))
import Maybe
import Row exposing (Row)
import String


type alias Matrix =
    Array Row


fromEntryLists : List (List Entry) -> Matrix
fromEntryLists lists =
    Array.fromList <|
        List.map
            Array.fromList
            lists


fromNumberLists : List (List Float) -> Matrix
fromNumberLists lists =
    fromEntryLists <|
        List.map
            (List.map <| Entry.fromFloat)
            lists


toRref : Matrix -> Matrix
toRref matrix =
    let
        toRrefHelper i1 m =
            if i1 >= Array.length m then
                m

            else
                let
                    row1 =
                        getRow i1 m
                    _ =
                        Debug.log "m" <| toString m
                in
                (case Row.getPivotIndex row1 of
                    Just p ->
                        let
                            pivot1 =
                                Row.getEntry p row1
                        in
                        (if Entry.equal pivot1 Entry.one then
                            identity

                        else
                            Array.set i1 (Row.scale (Entry.reciprocal pivot1) row1)
                        ) >>
                        Array.indexedMap
                            (\i2 row2 ->
                                let
                                    pivot2 =
                                        Row.getEntry p row2
                                in
                                if i2 == i1 then
                                    row2
                                else if Entry.equal pivot2 Entry.zero then
                                    row2
                                else
                                    Row.subtract row2 (Row.scale (Entry.divide pivot2 pivot1) row1)
                            )
                        >> toRrefHelper (i1 + 1)

                    Nothing ->
                        toRrefHelper (i1 + 1)
                )
                    m
    in
    sortUsingPivotIndex <| toRrefHelper 0 matrix


sortUsingPivotIndex : Matrix -> Matrix
sortUsingPivotIndex matrix =
    Array.fromList <|
        List.sortWith
            (\a b ->
                case ( Row.getPivotIndex a, Row.getPivotIndex b ) of
                    ( Just ia, Just ib ) ->
                        if ia < ib then
                            LT

                        else if ia == ib then
                            EQ

                        else
                            GT

                    ( Just _, Nothing ) ->
                        LT

                    ( Nothing, Just _ ) ->
                        GT

                    ( Nothing, Nothing ) ->
                        EQ
            )
            (Array.toList matrix)


toString : Matrix -> String
toString matrix =
    String.join "\n" <|
        Array.toList <|
            Array.map
                (\row ->
                    String.join
                        " "
                        (Array.Extra.mapToList Entry.toString row)
                )
                matrix


getRow : Int -> Matrix -> Row
getRow index matrix =
    Maybe.withDefault Array.empty <| Array.get index matrix
