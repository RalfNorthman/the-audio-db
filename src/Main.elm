module Main exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
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
    Decode.map TrackContainer
        (field "track" (oneOf [ null [], list trackDecoder ]))



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
        (field "album" (oneOf [ null [], list albumDecoder ]))



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
        (field "artists" (oneOf [ null [], list artistDecoder ]))



-- Model


type alias Model =
    { return : Return
    , inputArtist : String
    , inputAlbum : String
    }


type Return
    = InitialState
    | Error String
    | ArtistResult ArtistContainer
    | AlbumResult AlbumContainer
    | TrackResult TrackContainer


init : () -> ( Model, Cmd Msg )
init _ =
    ( { return = InitialState
      , inputArtist = ""
      , inputAlbum = ""
      }
    , request exampleQuery
    )



-- Update


type Msg
    = TrackRequest (Result Http.Error TrackContainer)
    | AlbumRequest (Result Http.Error AlbumContainer)
    | ArtistRequest (Result Http.Error ArtistContainer)
    | InputArtist String
    | InputAlbum String
    | ClickArtist
    | ClickAlbum


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrackRequest (Ok result) ->
            ( { model | return = TrackResult result }
            , Cmd.none
            )

        TrackRequest (Err _) ->
            ( { model | return = Error "track request error" }
            , Cmd.none
            )

        AlbumRequest (Ok result) ->
            ( { model | return = AlbumResult result }
            , Cmd.none
            )

        AlbumRequest (Err _) ->
            ( { model | return = Error "album request error" }
            , Cmd.none
            )

        ArtistRequest (Ok result) ->
            ( { model | return = ArtistResult result }
            , Cmd.none
            )

        ArtistRequest (Err _) ->
            ( { model | return = Error "artist request error" }
            , Cmd.none
            )

        InputArtist input ->
            ( { model | inputArtist = input }
            , Cmd.none
            )

        InputAlbum input ->
            ( { model | inputAlbum = input }
            , Cmd.none
            )

        ClickArtist ->
            ( model, request <| ArtistSearch model.inputArtist )

        ClickAlbum ->
            ( model, request <| AlbumSearch model.inputAlbum )



-- View helpers


googleFont : String -> Attribute Msg
googleFont fontName =
    let
        fontString =
            String.replace " " "+" fontName
    in
        Font.family
            [ Font.external
                { url =
                    "https://fonts.googleapis.com/css?family="
                        ++ fontString
                , name = fontName
                }
            ]


myText : String -> Element Msg
myText string =
    paragraph [ padding 10 ] [ text string ]


artistView : ArtistContainer -> Element Msg
artistView result =
    if List.isEmpty result.artist then
        myText "No artist found."
    else
        case List.head result.artist of
            Nothing ->
                myText "No artist found."

            Just artist ->
                column [ spacing 10, padding 10 ]
                    [ image []
                        { src = artist.logo
                        , description = ""
                        }
                    , myText artist.bio
                    , image []
                        { src = artist.image
                        , description = ""
                        }
                    ]


trackView : TrackContainer -> Element Msg
trackView result =
    if List.isEmpty result.track then
        myText "No track found."
    else
        case List.head result.track of
            Nothing ->
                myText "No track found."

            Just track ->
                [ track.artist
                , track.album
                , track.trackNumber
                , track.track
                ]
                    |> String.join " - "
                    |> myText


albumView : AlbumContainer -> Element Msg
albumView result =
    if List.isEmpty result.album then
        myText "No album with that name."
    else
        let
            stringify album =
                [ album.artist
                , album.album
                , album.year
                ]
                    |> String.join " - "

            makeRow album =
                row [ spacing 20 ]
                    [ image [ height <| px 200, width <| px 200 ]
                        { src = album.image ++ "/preview"
                        , description = ""
                        }
                    , myText <| stringify album
                    ]

            rowList =
                result.album
                    |> List.map makeRow
        in
            column [ spacing 10, padding 10 ] rowList


caseView : Return -> Element Msg
caseView return =
    case return of
        InitialState ->
            myText "Initial state."

        Error errorMessage ->
            myText errorMessage

        ArtistResult result ->
            artistView result

        TrackResult result ->
            trackView result

        AlbumResult result ->
            albumView result


myInput modelPart label msg clickMsg =
    let
        inputBox =
            Input.text
                [ width <| px 300 ]
                { onChange = (\x -> msg x)
                , text = modelPart
                , placeholder = Nothing
                , label =
                    Input.labelLeft [ moveDown 10 ] <|
                        text label
                }

        clickButton =
            Input.button []
                { onPress = clickMsg
                , label = text "submit"
                }
    in
        row [] [ inputBox, clickButton ]



-- View


exampleQuery : Query
exampleQuery =
    ArtistSearch "gwar"


view : Model -> Html Msg
view model =
    layout [ googleFont "Montserrat" ] <|
        column [ padding 10 ]
            [ myInput model.inputArtist "Artist:" InputArtist <|
                Just ClickArtist
            , myInput model.inputAlbum "Album:" InputAlbum <|
                Just ClickAlbum
            , caseView model.return
            ]



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
