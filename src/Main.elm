module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (href, src, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import String.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Page =
    { title : String
    , description : String
    , image : String
    , summary : String
    }


type alias Model =
    { loading : Bool
    , page : Page
    , article : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loading = True
      , page = Page "" "" "" ""
      , article = ""
      }
    , fetchPage
    )



-- UPDATE


type Msg
    = NewPage
    | GotPage (Result Http.Error Page)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPage ->
            ( { model | loading = True }
            , fetchPage
            )

        GotPage result ->
            case result of
                Ok page ->
                    if
                        String.startsWith "obec " page.description
                            || String.contains "obce " page.description
                    then
                        ( model, fetchPage )

                    else
                        ( { model
                            | loading = False
                            , page = Page page.title page.description page.image page.summary
                          }
                        , scrollToTop
                        )

                Err _ ->
                    ( { model
                        | loading = False
                        , page = Page "" "Omlouváme se, nejsme schopni uspokojit vaši touhu po vědomostech…" "" ""
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


fetchPage : Cmd Msg
fetchPage =
    Http.get
        { url = "https://cs.wikipedia.org/api/rest_v1/page/random/summary"
        , expect = Http.expectJson GotPage decodePage
        }


decodePage : Decoder Page
decodePage =
    Decode.succeed Page
        |> required "title" string
        |> required "description" string
        |> required "thumbnail"
            (field "source" string)
        |> required "extract" string


scrollToTop : Cmd Msg
scrollToTop =
    Task.perform
        (\_ -> NoOp)
        (Dom.setViewport 0 0)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        page =
            model.page
    in
    { title =
        "Triwia – "
            ++ (if model.loading then
                    "Loading…"

                else
                    page.title
               )
    , body =
        [ main_ []
            [ h1 [] [ text page.title ]
            , h3 [] [ text (String.Extra.toSentenceCase page.description) ]
            , img [ src page.image ] []
            , p [] [ text page.summary ]
            , a [ href ("https://cs.wikipedia.org/wiki/" ++ page.title), target "_blank" ] [ text "Pokračovat ve čtení na Wiki…" ]
            , button [ onClick NewPage ] [ text "Další článek" ]
            ]
        ]
    }
