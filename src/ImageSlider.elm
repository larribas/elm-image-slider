module ImageSlider exposing (Config, FocusedSlide, Msg(..), init, update, view)

{-| A component to display and navigate through an image carousel. See the [demo here](https://larribas.github.io/elm-image-slider/) to get a clear idea.

It supports moving via arrow keys, visual arrows and clicking on the image thumbnails. The default styles come with easy support for the traditional close button on the upper right corner.

You can supply any type to the carousel, as long as you can supply functions that extract each item's originalUrl, thumbnailUrl and caption.


# Main workflow

@docs Msg, FocusedSlide, Config, init, update, view

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


{-| Minimal model for the component. It remembers the currently focused image on the slider
-}
type alias FocusedSlide =
    Int


{-| Holds all necessary transformations to display the image slider
-}
type alias Config a =
    { originalUrl : a -> String
    , thumbnailUrl : a -> String
    , alt : a -> String
    , caption : a -> Html Msg
    }


{-| Initialize the component's state.
-}
init : Int -> ( FocusedSlide, Cmd Msg )
init i =
    ( i, Task.attempt (\_ -> ShowSlide i) (Dom.focus "image-slider-container") )


{-| Updates the component's state
-}
update : Msg -> FocusedSlide -> FocusedSlide
update msg focusedSlide =
    case msg of
        ShowSlide i ->
            i


{-| Renders the component visually. Should the slide array be empty, it displays nothing
-}
view : Config a -> Array a -> FocusedSlide -> Html Msg
view conf slides focusedSlide =
    let
        isFirstSlide =
            focusedSlide == 0

        length =
            Array.length slides

        isLastSlide =
            focusedSlide >= length - 1

        focused =
            if focusedSlide >= 0 && focusedSlide < length then
                focusedSlide
            else
                0

        previous =
            if focusedSlide > 0 then
                focusedSlide - 1
            else
                0

        next =
            if focusedSlide >= 0 && focusedSlide < length - 1 then
                focusedSlide + 1
            else
                length - 1
    in
    Html.div
        [ Attr.id "image-slider-container"
        , onKeyDown <| Dict.fromList [ ( 37, ShowSlide previous ), ( 39, ShowSlide next ) ]
        , Attr.tabindex 1
        ]
        [ Html.div [ Attr.class "image-slider-navigation-container" ]
            [ Html.i [ Attr.class "image-slider-button image-slider-previous-button fa fa-chevron-left", Event.onClick <| ShowSlide previous, Attr.classList [ ( "image-slider-hidden", isFirstSlide ) ] ] []
            , Html.div [ Attr.class "image-slider-image-container" ]
                (case Array.get focused slides of
                    Just image ->
                        [ Html.img [ Attr.class "image-slider-image-main", Attr.src (image |> conf.originalUrl), Attr.alt (image |> conf.alt) ] []
                        , image |> conf.caption
                        ]

                    Nothing ->
                        [ Html.span [ Attr.class "image-slider-no-images" ] [] ]
                )
            , Html.i [ Attr.class "image-slider-button image-slider-next-button fa fa-chevron-right", Event.onClick <| ShowSlide next, Attr.classList [ ( "image-slider-hidden", isLastSlide ) ] ] []
            ]
        , viewThumbnails conf slides focusedSlide
        ]


viewThumbnails : Config a -> Array a -> FocusedSlide -> Html Msg
viewThumbnails conf slides focusedSlide =
    let
        totalSlides =
            Array.length slides

        slidesToShow =
            let
                ( lowerIndex, upperIndex ) =
                    ( focusedSlide - 2, focusedSlide + 2 )
            in
            if totalSlides <= 5 then
                slides
            else if lowerIndex < 0 then
                slides |> Array.slice 0 4
            else if upperIndex >= totalSlides then
                slides |> Array.slice -4 totalSlides
            else
                slides |> Array.slice lowerIndex upperIndex
    in
    Html.div [ Attr.class "image-slider-all-images-container" ]
        (slidesToShow
            |> Array.indexedMap (viewThumbnail conf focusedSlide)
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
