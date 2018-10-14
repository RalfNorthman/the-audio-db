module Main exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Browser
import Http
import Url.Builder as Url exposing (QueryParameter)
import Json.Decode as Decode exposing (Decoder, int, string, list)
import Json.Decode.Pipeline exposing (required, optional, requiredAt)


-- Query Types


type alias ArtistName =
    String


type alias AlbumName =
    String


type alias TrackName =
    String


type Query
    = ArtistSearch ArtistName
    | ArtistAlbumSearch ArtistName AlbumName
    | AlbumSearch AlbumName
    | TrackSearch ArtistName TrackName
    | Discography ArtistName



-- Url Helpers


pasteUrl : String -> List QueryParameter -> String
pasteUrl requestType parameterList =
    let
        baseUrl =
            "https://www.theaudiodb.com/"

        apiKey =
            String.fromInt 1
    in
        baseUrl
            ++ Url.relative
                [ "api", "v1", "json", apiKey, requestType ]
                parameterList



-- Create a query


queryToUrl : Query -> String
queryToUrl query =
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
                pasteUrl "search.php"
                    [ artistParameter artist ]

            ArtistAlbumSearch artist album ->
                pasteUrl "searchalbum.php"
                    [ artistParameter artist
                    , albumParameter album
                    ]

            AlbumSearch album ->
                pasteUrl "searchalbum.php"
                    [ albumParameter album ]

            TrackSearch artist track ->
                pasteUrl "searchtrack.php"
                    [ artistParameter artist
                    , trackParameter track
                    ]

            Discography artist ->
                pasteUrl "discography.php"
                    [ artistParameter artist ]



-- Http


request : Query -> Cmd Msg
request query =
    let
        url =
            queryToUrl query
    in
        Http.send Request <|
            Http.get url trackContainerDecoder



-- Json


type alias Track =
    { artist : String
    , album : String
    , trackNumber : String
    , track : String
    }


type alias TrackContainer =
    { track : List Track
    }


trackDecoder : Decoder Track
trackDecoder =
    Decode.succeed Track
        |> required "strArtist" string
        |> required "strAlbum" string
        |> required "intTrackNumber" string
        |> required "strTrack" string


trackContainerDecoder : Decoder TrackContainer
trackContainerDecoder =
    Decode.succeed TrackContainer
        |> required "track" (list trackDecoder)



-- Model


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", request exampleQuery )



-- Update


type Msg
    = Submit
    | Request (Result Http.Error TrackContainer)


resultToString : TrackContainer -> String
resultToString result =
    case List.head result.track of
        Nothing ->
            "No Track"

        Just track ->
            [ track.artist
            , track.album
            , track.trackNumber
            , track.track
            ]
                |> String.join " - "


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model, request exampleQuery )

        Request (Ok result) ->
            ( resultToString result
            , Cmd.none
            )

        Request (Err _) ->
            ( "error", Cmd.none )



-- View


exampleQuery : Query
exampleQuery =
    TrackSearch "Front 242" "Tragedy for you"


view : Model -> Html msg
view model =
    layout [] <| text model



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
