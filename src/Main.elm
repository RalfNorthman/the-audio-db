module Main exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Browser
import Http
import Url.Builder as Url exposing (QueryParameter)
import Json.Decode as Decode exposing (Decoder, nullable, field, int, string, list, oneOf, null)
import Json.Decode.Pipeline exposing (required, optional, requiredAt)


-- Settings


defaultImage =
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


makeRequest query requestType decoder =
    let
        url =
            queryToUrl query
    in
        Http.send requestType <|
            Http.get url decoder


request : Query -> Cmd Msg
request query =
    case query of
        ArtistSearch _ ->
            makeRequest query ArtistRequest artistContainerDecoder

        AlbumSearch _ ->
            makeRequest query AlbumRequest albumContainerDecoder

        ArtistAlbumSearch _ _ ->
            makeRequest query AlbumRequest albumContainerDecoder

        TrackSearch _ _ ->
            makeRequest query TrackRequest trackContainerDecoder



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


maybeImage : Decoder String
maybeImage =
    oneOf [ string, null defaultImage ]


albumDecoder : Decoder Album
albumDecoder =
    Decode.map4 Album
        (field "strArtist" string)
        (field "strAlbum" string)
        (field "intYearReleased" string)
        (field "strAlbumThumb" maybeImage)


albumContainerDecoder : Decoder AlbumContainer
albumContainerDecoder =
    Decode.map AlbumContainer
        (field "album" (list albumDecoder))



-- Json Artist


type alias Artist =
    { artist : String
    , logo : String
    , image : String
    , bio : String
    }


type alias ArtistContainer =
    { artist : List Artist }


maybeString : Decoder String
maybeString =
    oneOf [ string, null "" ]


artistDecoder : Decoder Artist
artistDecoder =
    Decode.map4 Artist
        (field "strArtist" string)
        (field "strArtistLogo" maybeImage)
        (field "strArtistThumb" maybeImage)
        (field "strBiographyEN" maybeString)


artistContainerDecoder : Decoder ArtistContainer
artistContainerDecoder =
    Decode.map ArtistContainer
        (field "artists" (list artistDecoder))



-- Model


type alias Model =
    List String


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ "Initial State." ], request exampleQuery )



-- Update


type Msg
    = TrackRequest (Result Http.Error TrackContainer)
    | AlbumRequest (Result Http.Error AlbumContainer)
    | ArtistRequest (Result Http.Error ArtistContainer)


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
        stringify album =
            [ album.artist
            , album.album
            , album.year
            , album.image
            ]
                |> String.join " - "
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

        ArtistRequest (Ok result) ->
            ( [ "add some functionality here" ]
            , Cmd.none
            )

        ArtistRequest (Err _) ->
            ( [ "artist request error" ], Cmd.none )



-- View


exampleQuery : Query
exampleQuery =
    ArtistSearch "kent"


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
