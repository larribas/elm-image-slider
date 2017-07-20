module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ImageSlider
import Regex
import Test exposing (..)


basicModel : ImageSlider.Model Int
basicModel =
    let
        ( model, _ ) =
            ImageSlider.init [ 1, 2, 3 ] 1
    in
    model


suite : Test
suite =
    describe "ImageSlider"
        [ describe "init"
            [ test "the focused slide defaults to 0 when out of range" <|
                \_ ->
                    let
                        ( { focusedSlide }, _ ) =
                            ImageSlider.init [ 1, 2, 3 ] 10
                    in
                    Expect.equal 0 focusedSlide
            ]
        , describe "update"
            [ test "changes to a particular focused slide" <|
                \_ ->
                    let
                        { focusedSlide } =
                            ImageSlider.update (ImageSlider.ShowSlide 2) basicModel
                    in
                    Expect.equal 2 focusedSlide
            , test "goes to the next slide" <|
                \_ ->
                    let
                        { focusedSlide } =
                            ImageSlider.update ImageSlider.NextSlide basicModel
                    in
                    Expect.equal 2 focusedSlide
            , test "does not go to the next slide if it is at the end" <|
                \_ ->
                    let
                        { focusedSlide } =
                            ImageSlider.update ImageSlider.NextSlide { basicModel | focusedSlide = 2 }
                    in
                    Expect.equal 2 focusedSlide
            , test "goes to the previous slide" <|
                \_ ->
                    let
                        { focusedSlide } =
                            ImageSlider.update ImageSlider.PreviousSlide basicModel
                    in
                    Expect.equal 0 focusedSlide
            , test "does not go to the previous slide if it is at the beginning" <|
                \_ ->
                    let
                        { focusedSlide } =
                            ImageSlider.update ImageSlider.PreviousSlide { basicModel | focusedSlide = 0 }
                    in
                    Expect.equal 0 focusedSlide
            ]
        ]
