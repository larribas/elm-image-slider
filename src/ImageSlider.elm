module ImageSlider exposing (Config, Model, Msg(..), init, update, view)

{-| A component to display and navigate through an image carousel. See the [demo here](https://larribas.github.io/elm-image-slider/) to get a clear idea.

It supports moving via arrow keys, visual arrows and clicking on the image thumbnails. The default styles come with easy support for the traditional close button on the upper right corner.

You can supply any type to the carousel, as long as you can supply functions that extract each item's originalUrl, thumbnailUrl and caption.


# Main workflow

@docs Msg, Model, Config, init, update, view

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Json
import Task


{-| Internal messages to manage the component's state.
-}
type Msg
    = ShowSlide Int
    | NextSlide
    | PreviousSlide
    | NoOp


{-| Minimal model for the component. It remembers its current position given an array of slides. Slides can be any type, as long as you are able to supply the functions described in @Config
-}
type alias Model a =
    { focusedSlide : Int
    , slides : Array a
    }


{-| Holds all necessary transformations to display the image slider
-}
type alias Config a =
    { originalUrl : a -> String
    , thumbnailUrl : a -> String
    , alt : a -> String
    , caption : a -> Html Msg
    }


{-| Initialize the component's state with an (Array, Index) pair. Should the index be out of range, it defaults to the first element.
-}
init : List a -> Int -> ( Model a, Cmd Msg )
init slides i =
    ( { focusedSlide =
            if i >= 0 && i < List.length slides then
                i
            else
                0
      , slides = Array.fromList slides
      }
    , Task.attempt (\_ -> NoOp) (Dom.focus "image-slider-container")
    )


{-| Updates the component's state
-}
update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ShowSlide i ->
            { model | focusedSlide = i }

        NextSlide ->
            if model.focusedSlide + 1 >= Array.length model.slides then
                model
            else
                { model | focusedSlide = model.focusedSlide + 1 }

        PreviousSlide ->
            if model.focusedSlide - 1 < 0 then
                model
            else
                { model | focusedSlide = model.focusedSlide - 1 }

        NoOp ->
            model


{-| Renders the component visually. Should the slide array be empty, it displays nothing
-}
view : Config a -> Model a -> Html Msg
view conf model =
    let
        isFirstSlide =
            model.focusedSlide == 0

        isLastSlide =
            model.focusedSlide >= Array.length model.slides - 1
    in
    if Array.isEmpty model.slides then
        Html.text ""
    else
        Html.div
            [ Attr.id "image-slider-container"
            , onKeyDown <| Dict.fromList [ ( 37, PreviousSlide ), ( 39, NextSlide ) ]
            , Attr.tabindex 1
            ]
            [ Html.div [ Attr.class "image-slider-navigation-container" ]
                [ Html.i [ Attr.class "image-slider-button image-slider-previous-button fa fa-chevron-left", Event.onClick PreviousSlide, Attr.classList [ ( "image-slider-hidden", isFirstSlide ) ] ] []
                , Html.div [ Attr.class "image-slider-image-container" ]
                    (case Array.get model.focusedSlide model.slides of
                        Just image ->
                            [ Html.img [ Attr.src (image |> conf.originalUrl), Attr.alt (image |> conf.alt) ] []
                            , image |> conf.caption
                            ]

                        Nothing ->
                            [ Html.span [ Attr.class "image-slider-no-images" ] [] ]
                    )
                , Html.i [ Attr.class "image-slider-button image-slider-next-button fa fa-chevron-right", Event.onClick NextSlide, Attr.classList [ ( "image-slider-hidden", isLastSlide ) ] ] []
                ]
            , viewThumbnails conf model
            ]


viewThumbnails : Config a -> Model a -> Html Msg
viewThumbnails conf model =
    let
        totalSlides =
            Array.length model.slides

        slidesToShow =
            let
                ( lowerIndex, upperIndex ) =
                    ( model.focusedSlide - 2, model.focusedSlide + 2 )
            in
            if totalSlides <= 5 then
                model.slides
            else if lowerIndex < 0 then
                model.slides |> Array.slice 0 4
            else if upperIndex >= totalSlides then
                model.slides |> Array.slice -5 totalSlides
            else
                model.slides |> Array.slice lowerIndex upperIndex
    in
    Html.div [ Attr.class "image-slider-all-images-container" ]
        (slidesToShow
            |> Array.indexedMap (viewThumbnail conf model.focusedSlide)
            |> Array.toList
        )


viewThumbnail : Config a -> Int -> Int -> a -> Html Msg
viewThumbnail conf focusedSlide i image =
    Html.img [ Attr.src (image |> conf.thumbnailUrl), Attr.alt (image |> conf.alt), Attr.classList [ ( "image-slider-current-image", i == focusedSlide ) ], Event.onClick <| ShowSlide i ] []


{-| Utility function to bind several keys to an element, and only react to those specific keys
-}
onKeyDown : Dict Int msg -> Html.Attribute msg
onKeyDown keysAndMsgs =
    let
        handle keyCode =
            case Dict.get keyCode keysAndMsgs of
                Just msg ->
                    Json.succeed msg

                Nothing ->
                    Json.fail <| "Unexpected keyCode " ++ toString keyCode
    in
    Event.on "keydown" <| Json.andThen handle Event.keyCode
