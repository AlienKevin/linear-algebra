module Tests exposing (..)

import Expect
import Matrix exposing (..)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "rref"
        [ test "Single solution" <|
            \_ ->
                Expect.equal
                    (Matrix.toString <|
                        Matrix.fromNumberLists
                            [ [ 1, 0, 0, 11 ]
                            , [ 0, 1, 0, -4 ]
                            , [ 0, 0, 1, 3 ]
                            ]
                    )
                    (Matrix.toString <|
                        Matrix.toRref <|
                            Matrix.fromNumberLists
                                [ [ 2, 8, 4, 2 ]
                                , [ 2, 5, 1, 5 ]
                                , [ 4, 10, -1, 1 ]
                                ]
                    )
        , test "Infinitely many solutions" <|
            \_ ->
                Expect.equal
                    (Matrix.toString <|
                        Matrix.fromNumberLists
                            [ [ 1, 2, 0, 0, 3, 2 ]
                            , [ 0, 0, 1, 0, -1, 4 ]
                            , [ 0, 0, 0, 1, -2, 3 ]
                            , [ 0, 0, 0, 0, 0, 0 ]
                            ]
                    )
                    (Matrix.toString <|
                        Matrix.toRref <|
                            Matrix.fromNumberLists
                                [ [ 2, 4, -2, 2, 4, 2 ]
                                , [ 1, 2, -1, 2, 0, 4 ]
                                , [ 3, 6, -2, 1, 9, 1 ]
                                , [ 5, 10, -4, 5, 9, 9 ]
                                ]
                    )
        , test "No solutions" <|
            \_ ->
                Expect.equal
                    (Matrix.toString <|
                        Matrix.fromNumberLists
                            [ [ 1, 0, 0, 1, 0 ]
                            , [ 0, 1, 0, 2, 0 ]
                            , [ 0, 0, 1, 3, 0 ]
                            , [ 0, 0, 0, 0, 1 ]
                            ]
                    )
                    (Matrix.toString <|
                        Matrix.toRref <|
                            Matrix.fromNumberLists
                                [ [ 1, -3, 0, -5, -7 ]
                                , [ 3, -12, -2, -27, -33 ]
                                , [ -2, 10, 2, 24, 29 ]
                                , [ -1, 6, 1, 14, 17 ]
                                ]
                    )
        ]
