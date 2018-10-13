module Main exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Browser
import Url.Builder as Url


-- Url


baseUrl =
    "https://www.theaudiodb.com/"


apiKey =
    String.fromInt 1


queryUrl requestType parameterList =
    baseUrl
        ++ Url.relative
            [ "api", "v1", "json", apiKey, requestType ]
            parameterList


exampleQueryUrl =
    queryToString <| TrackSearch "Kent" "columbus"



-- Query Types


type alias Artist =
    String


type alias Album =
    String


type alias Track =
    String


type Query
    = ArtistSearch Artist
    | AlbumSearch Artist Album
    | TrackSearch Artist Track
    | Discography Artist


queryToString : Query -> String
queryToString query =
    let
        artistParameter name =
            Url.string "s" name

        albumParameter name =
            Url.string "a" name

        trackParameter name =
            Url.string "t" name
    in
        case query of
            ArtistSearch artist ->
                queryUrl "search.php"
                    [ artistParameter artist ]

            AlbumSearch artist album ->
                queryUrl "searchalbum.php"
                    [ artistParameter artist
                    , albumParameter album
                    ]

            TrackSearch artist track ->
                queryUrl "searchtrack.php"
                    [ artistParameter artist
                    , trackParameter track
                    ]

            Discography artist ->
                queryUrl "discography.php"
                    [ artistParameter artist ]



-- Model


type alias Model =
    {}


init : () -> ( Model, Cmd msg )
init _ =
    ( {}, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html msg
view model =
    layout [] <| text exampleQueryUrl



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Program


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
