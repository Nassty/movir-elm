module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Material
import Material.Scheme
import Material.Typography as Typography
import Material.Icon as Icon
import Material.Card as Card
import Material.Card as Card
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Layout as Layout
import Material.Spinner as Spinner
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Color as Color


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = GetFilms
    | GotFilms (Result Http.Error (List Movie))
    | GetPage Int
    | SortBy String
    | QueryTerm String
    | Mdl (Material.Msg Msg)


type alias TorrentLink =
    { url : String
    , size : String
    , quality : String
    }


type alias Movie =
    { id : Int
    , title : String
    , summary : String
    , cover : String
    , rating : Float
    , url : String
    , torrents : List TorrentLink
    , year : Int
    }


type alias Model =
    { loading : Bool
    , movie : List Movie
    , error : String
    , mdl : Material.Model
    , currentPage : Int
    , sort_by : String
    , query_term : String
    }


init : ( Model, Cmd Msg )
init =
    ( (Model True [] "" Material.model 1 "rating" ""), goGetFilms 1 "rating" "" )


type alias Mdl =
    Material.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetFilms ->
            ( { model | loading = True, movie = [] }, goGetFilms model.currentPage model.sort_by model.query_term )

        GetPage page ->
            ( { model | loading = True, movie = [], currentPage = page }, goGetFilms page model.sort_by model.query_term )

        GotFilms (Ok movies) ->
            ( { model | loading = False, movie = movies }, Cmd.none )

        GotFilms (Err err) ->
            ( { model | loading = False, movie = [], error = (toString err) }, Cmd.none )

        SortBy criteria ->
            ( { model | loading = True, movie = [], sort_by = criteria }, goGetFilms model.currentPage criteria model.query_term )

        QueryTerm term ->
            ( { model | query_term = term }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


white : Options.Property c m
white =
    Color.text Color.white


renderMovie : Movie -> Material.Grid.Cell Msg
renderMovie movie =
    cell [ Material.Grid.offset All 2, Material.Grid.size All 12 ]
        [ Card.view
            [ Options.css "width" "750px"
            , Options.css "height" "350px"
            , Options.css "background" ("url('" ++ movie.cover ++ "') center / cover")
            ]
            [ Card.title
                [ white
                , Typography.title
                , Typography.contrast 1.0
                , Options.css "width" "100%"
                , Options.css "padding" "0"
                , Options.css "margin" "0"
                ]
                [ Card.head
                    [ Options.css "background" "rgba(0, 0, 0, 0.5)"
                    , Options.css "width" "100%"
                    , Options.css "padding" "30px"
                    ]
                    -- Non-gradient scrim
                    [ Options.span
                        []
                        [ text (movie.title ++ " (" ++ (toString movie.year) ++ ")") ]
                    ]
                ]
            , Card.text [ Card.expand ] []
              -- Filler
            , Card.text
                [ Options.css "background" "rgba(0, 0, 0, 0.5)"
                , Options.css "width" "100%"
                ]
                -- Non-gradient scrim
                [ Options.span
                    [ white
                    , Typography.contrast 1.0
                    , Options.css "width" "100%"
                    ]
                    (List.map renderTorrent movie.torrents)
                ]
            , Card.text
                [ Options.css "background" "rgba(0, 0, 0, 0.5)"
                , Options.css "padding" "5px"
                , Options.css "width" "100%"
                ]
                [ Options.span
                    [ white, Typography.contrast 1.0 ]
                    [ text (movie.summary) ]
                ]
            ]
        ]



--   [ h2 [] [ text (movie.title ++ " (" ++ (toString movie.year) ++ ")") ]
--   , grid []
--       [ cell [ Material.Grid.size All 2 ]
--           [ img [ src movie.cover ] []
--           , p [] [ text (toString movie.rating ++ "/10") ]
--           ]
--       , cell [ Material.Grid.size All 4, Material.Grid.offset All 1 ]
--           [ p [] (List.append (List.map renderTorrent movie.torrents) [ (text movie.summary) ]) ]
--       ]
--   ]


renderTorrent : TorrentLink -> Html Msg
renderTorrent model =
    a [ href model.url, target "_blank" ] [ Icon.i "file_download", text (model.quality ++ " (" ++ model.size ++ ") ") ]


renderButtons : Model -> Html Msg
renderButtons model =
    let
        nextPage =
            model.currentPage + 1

        prevPage =
            (Basics.max 1 (model.currentPage - 1))
    in
        grid []
            [ cell [ Material.Grid.offset All 2, Material.Grid.size All 12 ]
                [ div []
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Options.onClick (GetPage prevPage)
                        , Options.disabled (model.loading || model.currentPage == prevPage)
                        ]
                        [ text ("<" ++ (toString prevPage)) ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Options.disabled True
                        ]
                        [ text (toString model.currentPage) ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Options.onClick (GetPage nextPage)
                        , Options.disabled model.loading
                        ]
                        [ text ((toString nextPage) ++ ">") ]
                    ]
                ]
            , cell [ Material.Grid.offset All 2, Material.Grid.size All 12 ] [ renderSearch model ]
            , cell [ Material.Grid.offset All 2, Material.Grid.size All 12 ] (renderSortButtons model)
            ]


renderSearch : Model -> Html Msg
renderSearch model =
    Html.form [ onSubmit GetFilms ]
        [ Textfield.render Mdl
            [ 99 ]
            model.mdl
            [ Textfield.label "Search Movie"
            , Textfield.floatingLabel
            , Textfield.value model.query_term
            , Options.disabled model.loading
            , Options.onInput QueryTerm
            ]
            []
        ]


sortButtons : List ( String, String )
sortButtons =
    [ ( "Title", "title" )
    , ( "Year", "year" )
    , ( "Rating", "rating" )
    , ( "Peers", "peers" )
    , ( "Seeds", "seeds" )
    , ( "Download Count", "download_count" )
    , ( "Like Count", "like_count" )
    , ( "Date Added", "date_added" )
    ]


renderFilterButton : Model -> Int -> ( String, String ) -> Html Msg
renderFilterButton model i ( name, filter ) =
    Button.render Mdl
        [ i + 2 ]
        model.mdl
        [ Button.raised
        , Button.ripple
        , Options.onClick (SortBy filter)
        , Button.accent |> Options.when (filter == model.sort_by)
        ]
        [ text (name) ]


renderSortButtons : Model -> List (Html Msg)
renderSortButtons model =
    List.indexedMap (renderFilterButton model) sortButtons


renderBody : Model -> Html Msg
renderBody model =
    if model.loading then
        grid []
            [ cell [ Material.Grid.offset All 2, Material.Grid.size All 12 ]
                [ div [] [ Spinner.spinner [ Spinner.active model.loading ] ]
                ]
            ]
    else
        grid [] (List.map renderMovie model.movie)


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = [ h2 [] [ text ("Movir") ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ renderButtons model, renderBody model ]
        }
        |> Material.Scheme.top


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


goGetFilms : Int -> String -> String -> Cmd Msg
goGetFilms page criteria query_term =
    let
        url =
            ("https://yts.ag/api/v2/list_movies.json?limit=50&sort_by="
                ++ criteria
                ++ "&page="
                ++ (toString page)
                ++ "&query_term="
                ++ query_term
            )
    in
        Http.send GotFilms (Http.get url decodeFilms)


decodeFilms : Decode.Decoder (List Movie)
decodeFilms =
    Decode.at [ "data", "movies" ] (Decode.list decodeFilm)


decodeFilm : Decode.Decoder Movie
decodeFilm =
    Decode.map8 Movie
        (Decode.at [ "id" ] Decode.int)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "description_full" ] Decode.string)
        (Decode.at [ "large_cover_image" ] Decode.string)
        (Decode.at [ "rating" ] Decode.float)
        (Decode.at [ "url" ] Decode.string)
        (Decode.at [ "torrents" ] (Decode.list getTorrents))
        (Decode.at [ "year" ] Decode.int)


getTorrents : Decode.Decoder TorrentLink
getTorrents =
    Decode.map3 TorrentLink
        (Decode.field "url" Decode.string)
        (Decode.field "size" Decode.string)
        (Decode.field "quality" Decode.string)
