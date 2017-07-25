module Demo exposing (main)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import ImageSlider
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Image =
    { original : String
    , thumbnail : String
    , source : String
    }


type alias Model =
    { images : List Image
    , slider : Maybe ImageSlider.FocusedSlide
    }


type Msg
    = OpenSlider Int
    | CloseSlider
    | SliderMsg ImageSlider.Msg


init : ( Model, Cmd Msg )
init =
    ( { images = exampleImages, slider = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenSlider i ->
            let
                ( sliderModel, cmd ) =
                    ImageSlider.init i
            in
            ( { model | slider = Just sliderModel }, Cmd.map SliderMsg cmd )

        SliderMsg sliderMsg ->
            case model.slider of
                Just focusedSlide ->
                    ( { model | slider = Just (ImageSlider.update sliderMsg focusedSlide) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CloseSlider ->
            ( { model | slider = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        toImg index { source, thumbnail } =
            Html.img
                [ Attr.src thumbnail
                , Attr.alt source
                , Ev.onClick <| OpenSlider index
                ]
                []

        sliderConf =
            { originalUrl = .original
            , thumbnailUrl = .thumbnail
            , alt = \_ -> "Some awesome giga pudding gif"
            , caption =
                \i ->
                    Html.span
                        [ Attr.class "image-slider-image-caption" ]
                        [ Html.text "From "
                        , Html.a [ Attr.href i.source ] [ Html.text i.source ]
                        ]
            }
    in
    Html.div []
        [ Html.div [ Attr.class "image-gallery" ] (List.indexedMap toImg exampleImages)
        , case model.slider of
            Just focusedSlide ->
                Html.div
                    [ onEscape CloseSlider ]
                    [ ImageSlider.view sliderConf (Array.fromList model.images) focusedSlide |> Html.map SliderMsg
                    , Html.i [ Attr.class "image-slider-button image-slider-quit-button fa fa-remove", Ev.onClick CloseSlider ] []
                    ]

            Nothing ->
                Html.text ""
        ]


onEscape : msg -> Html.Attribute msg
onEscape msg =
    let
        handle keyCode =
            if keyCode == 27 then
                Json.succeed msg
            else
                Json.fail <| "Unexpected keyCode " ++ toString keyCode
    in
    Ev.on "keydown" <| Json.andThen handle Ev.keyCode


exampleImages : List Image
exampleImages =
    [ { source = "http://thisisjusttosayihave.tumblr.com/post/49240224230/for-sarah", original = "https://media1.giphy.com/media/qTIrgoJ9FkfQs/giphy.gif", thumbnail = "https://media2.giphy.com/media/qTIrgoJ9FkfQs/giphy-downsized_s.gif" }, { source = "http://kyle2756.tumblr.com/post/68397958497/really-want-to-try-one-of-these", original = "https://media0.giphy.com/media/diMYD07ksTS7K/giphy.gif", thumbnail = "https://media1.giphy.com/media/diMYD07ksTS7K/giphy-downsized_s.gif" }, { source = "http://estranhamente-louco.tumblr.com/post/48813784199", original = "https://media3.giphy.com/media/tHdwiQUm1qQb6/giphy.gif", thumbnail = "https://media3.giphy.com/media/tHdwiQUm1qQb6/giphy_s.gif" }, { source = "http://angelum-scum.tumblr.com/post/55196509619", original = "https://media0.giphy.com/media/rVLth3BIx2Zby/giphy.gif", thumbnail = "https://media3.giphy.com/media/rVLth3BIx2Zby/giphy-downsized_s.gif" }, { source = "http://pudim-com-ketchup.tumblr.com/post/38147229468", original = "https://media4.giphy.com/media/9uuGyMaI6ob0Q/giphy.gif", thumbnail = "https://media3.giphy.com/media/9uuGyMaI6ob0Q/giphy-downsized_s.gif" }, { source = "http://thecabbageguy.tumblr.com/post/48987810170/giga-pudding-puripuri-puripuri-yo-buddy-party", original = "https://media3.giphy.com/media/nHTBRh2v0714s/giphy.gif", thumbnail = "https://media2.giphy.com/media/nHTBRh2v0714s/giphy-downsized_s.gif" }, { source = "http://oolonge.tumblr.com/post/52673262869/i-just-realllllllllllllly-want-pudding", original = "https://media0.giphy.com/media/QfUwLcZ1cbaOQ/giphy.gif", thumbnail = "https://media0.giphy.com/media/QfUwLcZ1cbaOQ/giphy-downsized_s.gif" }, { source = "http://kyle2756.tumblr.com/post/68397958497/really-want-to-try-one-of-these", original = "https://media4.giphy.com/media/8n1tFqaYgJq8g/giphy.gif", thumbnail = "https://media0.giphy.com/media/8n1tFqaYgJq8g/giphy-downsized_s.gif" }, { source = "http://goshidolovefood.tumblr.com/post/61934799450/giga-pudding", original = "https://media2.giphy.com/media/P4rJ3PHSYnpkY/giphy.gif", thumbnail = "https://media2.giphy.com/media/P4rJ3PHSYnpkY/giphy_s.gif" }, { source = "http://fyeahgigapudding.tumblr.com/post/37690375764", original = "https://media2.giphy.com/media/xyhCdfvmx5EwU/giphy.gif", thumbnail = "https://media2.giphy.com/media/xyhCdfvmx5EwU/giphy-downsized_s.gif" }, { source = "http://nervous-nirvanna.tumblr.com/post/47223198946/puddi-puddi-giga-pudding", original = "https://media0.giphy.com/media/QTLyrtTKHimRO/giphy.gif", thumbnail = "https://media0.giphy.com/media/QTLyrtTKHimRO/giphy-downsized_s.gif" }, { source = "http://vctc.tumblr.com/post/76543422822/http-dagobah-net-flash-giga-pudding-swf", original = "https://media1.giphy.com/media/11Tx85m5mSnkLC/giphy.gif", thumbnail = "https://media3.giphy.com/media/11Tx85m5mSnkLC/giphy-downsized_s.gif" }, { source = "http://goshidolovefood.tumblr.com/post/61934799450/giga-pudding", original = "https://media2.giphy.com/media/a1gyIMHe0FCI8/giphy.gif", thumbnail = "https://media1.giphy.com/media/a1gyIMHe0FCI8/giphy-downsized_s.gif" }, { source = "http://kagurima.tumblr.com/post/59390563919", original = "https://media3.giphy.com/media/QbyROby3e6nT2/giphy.gif", thumbnail = "https://media2.giphy.com/media/QbyROby3e6nT2/giphy-downsized_s.gif" }, { source = "http://gisellejcat.tumblr.com/post/43576932569", original = "https://media4.giphy.com/media/tQJlq69VK9VXq/giphy.gif", thumbnail = "https://media4.giphy.com/media/tQJlq69VK9VXq/giphy-downsized_s.gif" }, { source = "http://goshidolovefood.tumblr.com/post/61934799450/giga-pudding", original = "https://media4.giphy.com/media/WQMyAdMpEPP7W/giphy.gif", thumbnail = "https://media4.giphy.com/media/WQMyAdMpEPP7W/giphy_s.gif" }, { source = "http://akagi-91.tumblr.com/post/61087289874/world-of-puddin", original = "https://media1.giphy.com/media/3kDUcDZJzyaiY/giphy.gif", thumbnail = "https://media3.giphy.com/media/3kDUcDZJzyaiY/giphy-downsized_s.gif" }, { source = "http://kagurima.tumblr.com/post/59390594759", original = "https://media1.giphy.com/media/oyF8jC2t9AuC4/giphy.gif", thumbnail = "https://media1.giphy.com/media/oyF8jC2t9AuC4/giphy_s.gif" }, { source = "http://4000mileseast.tumblr.com/post/7882000799/that-awkward-moment-when-you-are-eating-a-pudding", original = "https://media3.giphy.com/media/iqeH12uLLlPCo/giphy.gif", thumbnail = "https://media2.giphy.com/media/iqeH12uLLlPCo/giphy-downsized_s.gif" }, { source = "http://goshidolovefood.tumblr.com/post/61934799450/giga-pudding", original = "https://media2.giphy.com/media/JhD4CHGyMIkuc/giphy.gif", thumbnail = "https://media4.giphy.com/media/JhD4CHGyMIkuc/giphy-downsized_s.gif" }, { source = "http://pattiiihh.tumblr.com/post/29491153753/yaaay-super-kawaii", original = "https://media3.giphy.com/media/AodPff1DTJr2M/giphy.gif", thumbnail = "https://media0.giphy.com/media/AodPff1DTJr2M/giphy-downsized_s.gif" }, { source = "http://oolonge.tumblr.com/post/52673262869/i-just-realllllllllllllly-want-pudding", original = "https://media2.giphy.com/media/UiYwzaq7GmViM/giphy.gif", thumbnail = "https://media2.giphy.com/media/UiYwzaq7GmViM/giphy-downsized_s.gif" }, { source = "http://adriftingintokyo.tumblr.com/post/37469369754/giga-pudding-vs-normal-pudding", original = "https://media4.giphy.com/media/RwnoM2uvfwqcw/giphy.gif", thumbnail = "https://media4.giphy.com/media/RwnoM2uvfwqcw/giphy-downsized_s.gif" }, { source = "http://angelum-scum.tumblr.com/post/55196509619", original = "https://media4.giphy.com/media/RObU4AKeSTt8Q/giphy.gif", thumbnail = "https://media2.giphy.com/media/RObU4AKeSTt8Q/giphy-downsized_s.gif" }, { source = "http://confectioncorrections.tumblr.com/post/41652114572/x", original = "https://media4.giphy.com/media/IxGl611H6zI2I/giphy.gif", thumbnail = "https://media0.giphy.com/media/IxGl611H6zI2I/giphy-downsized_s.gif" } ]
