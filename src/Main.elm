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


request : Query -> Cmd Msg
request query =
    let
        makeRequest requestType decoder =
            let
                url =
                    queryToUrl query
            in
                Http.send requestType <|
                    Http.get url decoder
    in
        case query of
            ArtistSearch _ ->
                makeRequest ArtistRequest artistContainerDecoder

            AlbumSearch _ ->
                makeRequest AlbumRequest albumContainerDecoder

            ArtistAlbumSearch _ _ ->
                makeRequest AlbumRequest albumContainerDecoder

            TrackSearch _ _ ->
                makeRequest TrackRequest trackContainerDecoder



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


type Model
    = InitialState
    | Error String
    | ArtistResult ArtistContainer
    | AlbumResult AlbumContainer
    | TrackResult TrackContainer


init : () -> ( Model, Cmd Msg )
init _ =
    ( InitialState, request exampleQuery )



-- Update


type Msg
    = TrackRequest (Result Http.Error TrackContainer)
    | AlbumRequest (Result Http.Error AlbumContainer)
    | ArtistRequest (Result Http.Error ArtistContainer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrackRequest (Ok result) ->
            ( TrackResult result
            , Cmd.none
            )

        TrackRequest (Err _) ->
            ( Error "track request error", Cmd.none )

        AlbumRequest (Ok result) ->
            ( AlbumResult result
            , Cmd.none
            )

        AlbumRequest (Err _) ->
            ( Error "album request error", Cmd.none )

        ArtistRequest (Ok result) ->
            ( ArtistResult result
            , Cmd.none
            )

        ArtistRequest (Err _) ->
            ( Error "artist request error", Cmd.none )



-- View helpers


trackView : TrackContainer -> Element Msg
trackView result =
    case List.head result.track of
        Nothing ->
            text "No Track"

        Just track ->
            [ track.artist
            , track.album
            , track.trackNumber
            , track.track
            ]
                |> String.join " - "
                |> text


albumView : AlbumContainer -> Element Msg
albumView result =
    let
        stringify album =
            [ album.artist
            , album.album
            , album.year
            , album.image
            ]
                |> String.join " - "

        elementList =
            result.album
                |> List.map stringify
                |> List.map text
    in
        column [] elementList


caseView : Model -> Element Msg
caseView model =
    case model of
        InitialState ->
            text "Initial state."

        Error errorMessage ->
            text errorMessage

        ArtistResult _ ->
            text "Artist functionality goes here."

        TrackResult result ->
            trackView result

        AlbumResult result ->
            albumView result



-- View


exampleQuery : Query
exampleQuery =
    AlbumSearch "love"


view : Model -> Html Msg
view model =
    layout [] <|
        caseView model



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
