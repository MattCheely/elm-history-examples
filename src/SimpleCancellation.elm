module SimpleCancellation exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html, a, button, div, h1, input, li, ol, p, text, ul)
import Html.Attributes exposing (checked, href, type_)
import Html.Events exposing (onCheck, onClick)
import Url exposing (Url)



-- ROUTING


type Page
    = Page1
    | Page2
    | Page3
    | NotFound


type alias Route =
    { page : Page
    , url : Url
    }


routeFromUrl : Url -> Route
routeFromUrl url =
    case url.fragment of
        Just "page1" ->
            { page = Page1, url = url }

        Just "page2" ->
            { page = Page2, url = url }

        Just "page3" ->
            { page = Page3, url = url }

        _ ->
            { page = NotFound, url = url }


type RoutingResult
    = Continue
    | Cancel


onRouteChange : { current : Route, next : Route } -> Model -> ( Model, RoutingResult )
onRouteChange transition model =
    case model.navigationPrompt of
        NotRequired ->
            ( model, Continue )

        Required ->
            ( { model
                | navigationPrompt =
                    Prompting
                        "Are you sure you want to leave this page?"
                        transition.next
              }
            , Cancel
            )

        Prompting prompt _ ->
            ( { model | navigationPrompt = Prompting prompt transition.next }
            , Cancel
            )



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    , navigationPrompt : NavigationPrompt
    }


type NavigationPrompt
    = NotRequired
    | Required
    | Prompting String Route


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , route = routeFromUrl url
      , navigationPrompt = NotRequired
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest
    | PromptRequested Bool
    | TransitionConfirmed Route
    | TransitionCanceled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        UrlRequested (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        UrlChanged url ->
            handleUrlChange url model

        PromptRequested shouldPrompt ->
            if shouldPrompt then
                ( { model | navigationPrompt = Required }, Cmd.none )

            else
                ( { model | navigationPrompt = NotRequired }, Cmd.none )

        TransitionConfirmed route ->
            ( { model | navigationPrompt = NotRequired }
            , Browser.Navigation.pushUrl model.key (Url.toString route.url)
            )

        TransitionCanceled ->
            ( { model | navigationPrompt = NotRequired }, Cmd.none )


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        transition =
            { current = model.route, next = routeFromUrl url }
    in
    if transition.current == transition.next then
        ( model, Cmd.none )

    else
        case onRouteChange transition model of
            ( newModel, Cancel ) ->
                ( newModel
                , Browser.Navigation.replaceUrl model.key (Url.toString transition.current.url)
                )

            ( newModel, Continue ) ->
                ( { newModel | route = transition.next }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Title"
    , body =
        [ div []
            [ promptView model
            , navView
            ]
        , pageView model.route.page
        , appExplanation
        ]
    }


promptView : Model -> Html Msg
promptView model =
    case model.navigationPrompt of
        Prompting prompt route ->
            div []
                [ text "Are you sure you want to leave this page?"
                , button [ onClick (TransitionConfirmed route) ] [ text "Yes" ]
                , button [ onClick TransitionCanceled ] [ text "No" ]
                ]

        _ ->
            div []
                [ text "Prompt before navigation?"
                , input
                    [ type_ "checkbox"
                    , onCheck PromptRequested
                    , checked (model.navigationPrompt == Required)
                    ]
                    []
                ]


navView : Html Msg
navView =
    ul []
        [ li [] [ a [ href "#page1" ] [ text "Page 1" ] ]
        , li [] [ a [ href "#page2" ] [ text "Page 2" ] ]
        , li [] [ a [ href "#page3" ] [ text "Page 3" ] ]
        ]


pageView : Page -> Html Msg
pageView page =
    div []
        [ h1 []
            [ case page of
                Page1 ->
                    text "Page 1"

                Page2 ->
                    text "Page 2"

                Page3 ->
                    text "Page 3"

                NotFound ->
                    text "Not Found"
            ]
        ]


appExplanation : Html Msg
appExplanation =
    div []
        [ p []
            [ text """
            This app demonstrates a naive approach to cancelling page transitions.
            The approach may be "good enough" for many use cases, but has a bug
            related to navigating with the back button
            """ ]
        , p []
            [ text "To see the problem with cancelling navigation and the back button:"
            ]
        , ol []
            [ li [] [ text "Start on Page 1" ]
            , li [] [ text "Navigate to Page 2" ]
            , li [] [ text "Navigate to Page 3" ]
            , li [] [ text "Enable the navigation prompt" ]
            , li [] [ text "Press the browser back button" ]
            , li [] [ text "Cancel navigation" ]
            , li [] [ text "Press the browser back button again" ]
            , li [] [ text "Notice you are on Page 1 because Page 2 got replaced with Page 3 on cancellation" ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , subscriptions = always Sub.none
        }
