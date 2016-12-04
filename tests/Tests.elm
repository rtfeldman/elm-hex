module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, string, intRange)
import String
import Hex


all : Test
all =
    describe "Hex"
        [ describe "to and from string"
            [ fuzz int "it works" <|
                \num ->
                    num
                        |> Hex.toString
                        |> Hex.fromString
                        |> Expect.equal (Ok num)
            ]
        , describe "toString"
            [ test "for 0" <|
                \() ->
                    0
                        |> Hex.toString
                        |> Expect.equal "0"
            , test "for 1" <|
                \() ->
                    1
                        |> Hex.toString
                        |> Expect.equal "1"
            , test "for 2" <|
                \() ->
                    2
                        |> Hex.toString
                        |> Expect.equal "2"
            , test "for 3" <|
                \() ->
                    3
                        |> Hex.toString
                        |> Expect.equal "3"
            , test "for 4" <|
                \() ->
                    4
                        |> Hex.toString
                        |> Expect.equal "4"
            , test "for 5" <|
                \() ->
                    5
                        |> Hex.toString
                        |> Expect.equal "5"
            , test "for 6" <|
                \() ->
                    6
                        |> Hex.toString
                        |> Expect.equal "6"
            , test "for 7" <|
                \() ->
                    7
                        |> Hex.toString
                        |> Expect.equal "7"
            , test "for 8" <|
                \() ->
                    8
                        |> Hex.toString
                        |> Expect.equal "8"
            , test "for 9" <|
                \() ->
                    9
                        |> Hex.toString
                        |> Expect.equal "9"
            , test "for 10" <|
                \() ->
                    10
                        |> Hex.toString
                        |> Expect.equal "a"
            , test "for 11" <|
                \() ->
                    11
                        |> Hex.toString
                        |> Expect.equal "b"
            , test "for 12" <|
                \() ->
                    12
                        |> Hex.toString
                        |> Expect.equal "c"
            , test "for 13" <|
                \() ->
                    13
                        |> Hex.toString
                        |> Expect.equal "d"
            , test "for 14" <|
                \() ->
                    14
                        |> Hex.toString
                        |> Expect.equal "e"
            , test "for 15" <|
                \() ->
                    15
                        |> Hex.toString
                        |> Expect.equal "f"
            ]
        , describe "fromHex"
            [ test "with an empty string" <|
                \() ->
                    ""
                        |> Hex.fromString
                        |> expectErr
            , describe "with one character"
                [ test "'0' works as expected" <|
                    \() ->
                        "0"
                            |> Hex.fromString
                            |> Expect.equal (Ok 0)
                , test "'1' works as expected" <|
                    \() ->
                        "1"
                            |> Hex.fromString
                            |> Expect.equal (Ok 1)
                , test "'2' works as expected" <|
                    \() ->
                        "2"
                            |> Hex.fromString
                            |> Expect.equal (Ok 2)
                , test "'3' works as expected" <|
                    \() ->
                        "3"
                            |> Hex.fromString
                            |> Expect.equal (Ok 3)
                , test "'4' works as expected" <|
                    \() ->
                        "4"
                            |> Hex.fromString
                            |> Expect.equal (Ok 4)
                , test "'5' works as expected" <|
                    \() ->
                        "5"
                            |> Hex.fromString
                            |> Expect.equal (Ok 5)
                , test "'6' works as expected" <|
                    \() ->
                        "6"
                            |> Hex.fromString
                            |> Expect.equal (Ok 6)
                , test "'7' works as expected" <|
                    \() ->
                        "7"
                            |> Hex.fromString
                            |> Expect.equal (Ok 7)
                , test "'8' works as expected" <|
                    \() ->
                        "8"
                            |> Hex.fromString
                            |> Expect.equal (Ok 8)
                , test "'9' works as expected" <|
                    \() ->
                        "9"
                            |> Hex.fromString
                            |> Expect.equal (Ok 9)
                , test "'a' works as expected" <|
                    \() ->
                        "a"
                            |> Hex.fromString
                            |> Expect.equal (Ok 10)
                , test "'b' works as expected" <|
                    \() ->
                        "b"
                            |> Hex.fromString
                            |> Expect.equal (Ok 11)
                , test "'c' works as expected" <|
                    \() ->
                        "c"
                            |> Hex.fromString
                            |> Expect.equal (Ok 12)
                , test "'d' works as expected" <|
                    \() ->
                        "d"
                            |> Hex.fromString
                            |> Expect.equal (Ok 13)
                , test "'e' works as expected" <|
                    \() ->
                        "e"
                            |> Hex.fromString
                            |> Expect.equal (Ok 14)
                , test "'f' works as expected" <|
                    \() ->
                        "f"
                            |> Hex.fromString
                            |> Expect.equal (Ok 15)
                , test "a known negative number works" <|
                    \() ->
                        "-abcdef01"
                            |> Hex.fromString
                            |> Expect.equal (Ok -2882400001)
                , test "a known hardcoded string works" <|
                    \() ->
                        "abcdef01"
                            |> Hex.fromString
                            |> Expect.equal (Ok 2882400001)
                , test "another known hardcoded string works" <|
                    \() ->
                        "a5"
                            |> Hex.fromString
                            |> Expect.equal (Ok 165)
                ]
            , describe "three-digit positive decimal numbers"
                [ fuzz3 (intRange 0 9) (intRange 0 9) (intRange 0 9) "numbers work" <|
                    \first second third ->
                        [ first, second, third ]
                            |> List.map toString
                            |> String.join ""
                            |> Hex.fromString
                            |> Expect.equal (Ok ((first * 256) + (second * 16) + third))
                ]
            , describe "three-digit negative decimal numbers"
                [ fuzz3 (intRange 0 9) (intRange 0 9) (intRange 0 9) "numbers work" <|
                    \first second third ->
                        [ first, second, third ]
                            |> List.map toString
                            |> String.join ""
                            |> ((++) "-")
                            |> Hex.fromString
                            |> Expect.equal (Ok (negate ((first * 256) + (second * 16) + third)))
                ]
            ]
        ]


expectErr : Result error value -> Expect.Expectation
expectErr val =
    case val of
        Err _ ->
            Expect.pass

        Ok okVal ->
            Expect.fail ("Expected an Err but got " ++ toString val)
