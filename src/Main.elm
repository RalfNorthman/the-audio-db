module Main exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Browser
import Http
import Url.Builder as Url exposing (QueryParameter)
import Json.Decode as Decode exposing (Decoder, int, string, list)
import Json.Decode.Pipeline exposing (required, optional, requiredAt)


-- Settings


defaultPicture =
    ""



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



-- Create a query url


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



-- Http


trackRequest : Query -> Cmd Msg
trackRequest query =
    let
        url =
            queryToUrl query
    in
        Http.send TrackRequest <|
            Http.get url trackContainerDecoder


albumRequest : Query -> Cmd Msg
albumRequest query =
    let
        url =
            queryToUrl query
    in
        Http.send AlbumRequest <|
            Http.get url albumContainerDecoder



-- Json Track


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



-- Json Album


type alias Album =
    { artist : String
    , album : String
    , year : String
    , image : String
    }


type alias AlbumContainer =
    { album : List Album }


albumDecoder : Decoder Album
albumDecoder =
    Decode.succeed Album
        |> required "strArtist" string
        |> required "strAlbum" string
        |> required "intYearReleased" string
        |> optional "strAlbumThumb" string defaultPicture


albumContainerDecoder : Decoder AlbumContainer
albumContainerDecoder =
    Decode.succeed AlbumContainer
        |> required "album" (list albumDecoder)



-- Model


type alias Model =
    List String


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ "Initial State." ], albumRequest exampleQuery )



-- Update


type Msg
    = TrackRequest (Result Http.Error TrackContainer)
    | AlbumRequest (Result Http.Error AlbumContainer)


tracksToString : TrackContainer -> List String
tracksToString result =
    case List.head result.track of
        Nothing ->
            "No Track"
                |> List.singleton

        Just track ->
            [ track.artist
            , track.album
            , track.trackNumber
            , track.track
            ]
                |> String.join " - "
                |> List.singleton


albumsToString : AlbumContainer -> List String
albumsToString result =
    let
        stringify =
            (\album ->
                [ album.artist
                , album.album
                , album.year
                , album.image
                ]
                    |> String.join " - "
            )
    in
        result.album
            |> List.map stringify


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrackRequest (Ok result) ->
            ( tracksToString result
            , Cmd.none
            )

        TrackRequest (Err _) ->
            ( [ "track request error" ], Cmd.none )

        AlbumRequest (Ok result) ->
            ( albumsToString result
            , Cmd.none
            )

        AlbumRequest (Err _) ->
            ( [ "album request error" ], Cmd.none )



-- View


exampleQuery : Query
exampleQuery =
    AlbumSearch "Alive"


view : Model -> Html msg
view model =
    layout [] <|
        column [] <|
            List.map text model



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
