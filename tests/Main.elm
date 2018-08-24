module Main exposing (basicConfig, click, expectOk, expectSelectedImage, has, hasNot, init, slidesOfSize, suite, thumbnailContainer)

import Array exposing (Array)
import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes as Attr
import ImageSlider
import Test exposing (..)
import Test.Html.Event as Event exposing (Event)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


suite : Test
suite =
    describe "ImageSlider"
        --The entire component's logic is on the view function, so it's the only one that I think would benefit from tests
        [ describe "view"
            [ test "it defaults to index 0 when out of range" <|
                \_ ->
                    ImageSlider.view basicConfig (slidesOfSize 3) (init 100)
                        |> expectSelectedImage 0
            , test "it goes to the next slide when clicking on the button" <|
                \_ ->
                    [ Selector.class "image-slider-next-button" ]
                        |> click basicConfig (slidesOfSize 3) 0
                        |> expectOk (expectSelectedImage 1)
            , test "it goes to the previous slide when clicking on the button" <|
                \_ ->
                    [ Selector.class "image-slider-previous-button" ]
                        |> click basicConfig (slidesOfSize 3) 2
                        |> expectOk (expectSelectedImage 1)
            , test "it cannot go to the next slide from the last one" <|
                \_ ->
                    [ Selector.class "image-slider-next-button" ]
                        |> click basicConfig (slidesOfSize 3) 2
                        |> expectOk (expectSelectedImage 2)
            , test "it cannot go to the previous slide from the first one" <|
                \_ ->
                    [ Selector.class "image-slider-previous-button" ]
                        |> click basicConfig (slidesOfSize 3) 0
                        |> expectOk (expectSelectedImage 0)
            , test "it shows an is-empty message when the array is empty" <|
                \_ ->
                    ImageSlider.view basicConfig Array.empty (init 0)
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.class "image-slider-no-images" ]
                        |> Query.count (Expect.equal 1)
            , test "it does not show an is-empty message when the array is not empty" <|
                \_ ->
                    ImageSlider.view basicConfig (slidesOfSize 3) (init 0)
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.class "image-slider-no-images" ]
                        |> Query.count (Expect.equal 0)
            , test "it shows the first thumbnails when the index is at the beginning" <|
                \_ ->
                    thumbnailContainer 30 1 |> Expect.all [ has 1, has 2, has 3, has 4, hasNot 5 ]
            , test "it shows the last thumbnails when the index is at the end" <|
                \_ ->
                    thumbnailContainer 30 28 |> Expect.all [ has 30, has 29, has 28, has 27, hasNot 26 ]
            , test "it shows a few thumbnails in the middle" <|
                \_ ->
                    thumbnailContainer 30 20 |> Expect.all [ hasNot 18, has 19, has 20, has 21, has 22, hasNot 23 ]
            ]
        ]


slidesOfSize : Int -> Array Int
slidesOfSize i =
    List.range 1 i |> Array.fromList


basicConfig : ImageSlider.Config Int
basicConfig =
    { originalUrl = toString
    , thumbnailUrl = toString
    , alt = toString
    , caption = toString >> Html.text
    }


init : Int -> ImageSlider.State
init =
    ImageSlider.init >> Tuple.first


thumbnailContainer : Int -> Int -> Query.Single ImageSlider.Msg
thumbnailContainer size focusedSlide =
    ImageSlider.view basicConfig (slidesOfSize size) (init focusedSlide)
        |> Query.fromHtml
        |> Query.find [ Selector.class "image-slider-all-images-container" ]


has : Int -> Query.Single ImageSlider.Msg -> Expectation
has i =
    Query.has [ Selector.tag "img", Selector.attribute <| Attr.src <| toString i ]


hasNot : Int -> Query.Single ImageSlider.Msg -> Expectation
hasNot i =
    Query.hasNot [ Selector.tag "img", Selector.attribute <| Attr.src <| toString i ]


click : ImageSlider.Config a -> Array a -> Int -> List Selector -> Result String (Html ImageSlider.Msg)
click config slides focusedSlide clickSelector =
    let
        state =
            init focusedSlide
    in
    ImageSlider.view config slides state
        |> Query.fromHtml
        |> Query.find clickSelector
        |> Event.simulate Event.click
        |> Event.toResult
        |> Result.map
            (\msg ->
                ImageSlider.update msg state
                    |> ImageSlider.view config slides
            )


expectOk : (a -> Expectation) -> Result err a -> Expectation
expectOk f result =
    case result of
        Ok value ->
            f value

        Err err ->
            Expect.fail <| "error: " ++ toString err


expectSelectedImage : Int -> Html ImageSlider.Msg -> Expectation
expectSelectedImage index =
    Query.fromHtml
        >> Query.find [ Selector.class "image-slider-image-main" ]
        >> Query.has
            [ Selector.tag "img"
            , Selector.attribute <| Attr.src <| toString (index + 1)
            ]
