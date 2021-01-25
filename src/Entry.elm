module Entry exposing (..)

import Arithmetic
import Fraction exposing (Fraction)


type Entry
    = Number Fraction


zero : Entry
zero =
    Number (Fraction.createUnsafe 0 1)


one : Entry
one =
    Number (Fraction.createUnsafe 1 1)


equal : Entry -> Entry -> Bool
equal e1 e2 =
    case ( e1, e2 ) of
        ( Number a, Number b ) ->
            Fraction.equal a b


subtract : Entry -> Entry -> Entry
subtract e1 e2 =
    case ( e1, e2 ) of
        ( Number a, Number b ) ->
            Number (Fraction.subtract a b)


multiply : Entry -> Entry -> Entry
multiply e1 e2 =
    case ( e1, e2 ) of
        ( Number a, Number b ) ->
            Number (Fraction.multiply a b)


divide : Entry -> Entry -> Entry
divide e1 e2 =
    case ( e1, e2 ) of
        ( Number a, Number b ) ->
            case Fraction.divide a b of
                Just result ->
                    Number result

                Nothing ->
                    one



-- impossible iff never divide by 0


reciprocal : Entry -> Entry
reciprocal e =
    case e of
        Number fraction ->
            case Fraction.reciprocal fraction of
                Just result ->
                    Number result

                Nothing ->
                    one



-- impossible iff never divide by 0


fromFloat : Float -> Entry
fromFloat float =
    Number <|
        fractionFromFloat float


fractionFromFloat : Float -> Fraction
fractionFromFloat float =
    if float == toFloat (truncate float) then
        Fraction.createUnsafe (truncate float) 1

    else
        let
            precision =
                10

            denominator =
                10 ^ precision

            numerator =
                round (float * denominator)

            gcd =
                Arithmetic.gcd numerator denominator
        in
        Fraction.createUnsafe
            (numerator // gcd)
            (denominator // gcd)


toString : Entry -> String
toString e =
    case e of
        Number fraction ->
            String.fromFloat <| Fraction.toFloat fraction
