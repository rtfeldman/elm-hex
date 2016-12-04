module Hex exposing (fromString)

{-| Convert a
-}


fromString : String -> Result String Int
fromString str =
    if String.isEmpty str then
        Err "Empty strings are not valid hexadecimal strings."
    else
        let
            result =
                if String.startsWith "-" str then
                    let
                        list =
                            str
                                |> String.toList
                                |> List.tail
                                |> Maybe.withDefault []
                    in
                        fromStringHelp (List.length list - 1) list 0
                            |> Result.map negate
                else
                    fromStringHelp (String.length str - 1) (String.toList str) 0

            formatError err =
                String.join " "
                    [ toString str
                    , "is not a valid hexadecimal string because"
                    , err
                    ]
        in
            Result.mapError formatError result


fromStringHelp : Int -> List Char -> Int -> Result String Int
fromStringHelp position chars accumulated =
    case chars of
        [] ->
            Ok accumulated

        char :: rest ->
            let
                recurse additional =
                    fromStringHelp
                        (position - 1)
                        rest
                        (accumulated + (additional * (16 ^ position)))
            in
                case char of
                    '0' ->
                        recurse 0

                    '1' ->
                        recurse 1

                    '2' ->
                        recurse 2

                    '3' ->
                        recurse 3

                    '4' ->
                        recurse 4

                    '5' ->
                        recurse 5

                    '6' ->
                        recurse 6

                    '7' ->
                        recurse 7

                    '8' ->
                        recurse 8

                    '9' ->
                        recurse 9

                    'a' ->
                        recurse 10

                    'b' ->
                        recurse 11

                    'c' ->
                        recurse 12

                    'd' ->
                        recurse 13

                    'e' ->
                        recurse 14

                    'f' ->
                        recurse 15

                    nonHex ->
                        Err (toString nonHex ++ " is not a valid hexadecimal character.")
