port module SmartCancellation exposing (main)

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


{-| Representation of a transition between routes. In a real app, the delta
and any othe details of history management should probably be behind an opaque type.

It might be better to track the history positions for both the current and next
routes. That could let the app prevent a call to redoTransition from the wrong
starting point in history, if the app keeps a transition around too long.

-}
type alias Transition =
    { previous : Route
    , next : Route
    , delta : Int
    }


type RoutingResult
    = Continue
    | Cancel


onRouteChange : Transition -> Model -> ( Model, RoutingResult )
onRouteChange transition model =
    case model.navigationPrompt of
        NotRequired ->
            ( model, Continue )

        Required ->
            ( { model
                | navigationPrompt =
                    Prompting
                        "Are you sure you want to leave this page?"
                        transition
              }
            , Cancel
            )

        Prompting prompt _ ->
            ( { model | navigationPrompt = Prompting prompt transition }
            , Cancel
            )



-- MODEL


type alias Model =
    { route : Route
    , navigationPrompt : NavigationPrompt
    }


type NavigationPrompt
    = NotRequired
    | Required
    | Prompting String Transition


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    ( { route = routeFromUrl url
      , navigationPrompt = NotRequired
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlChanged Url
    | UrlChangeWithDelta { url : Url, delta : Int }
    | UrlRequested UrlRequest
    | PromptRequested Bool
    | TransitionConfirmed Transition
    | TransitionCanceled
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, pushUrl (Url.toString url) )

        UrlRequested (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        UrlChanged url ->
            handleUrlChange url 1 model

        UrlChangeWithDelta { url, delta } ->
            handleUrlChange url delta model

        PromptRequested shouldPrompt ->
            if shouldPrompt then
                ( { model | navigationPrompt = Required }, Cmd.none )

            else
                ( { model | navigationPrompt = NotRequired }, Cmd.none )

        TransitionConfirmed transition ->
            ( { model | navigationPrompt = NotRequired }
            , redoTransition transition
            )

        TransitionCanceled ->
            ( { model | navigationPrompt = NotRequired }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleUrlChange : Url -> Int -> Model -> ( Model, Cmd Msg )
handleUrlChange url delta model =
    let
        transition =
            { previous = model.route
            , next = routeFromUrl url
            , delta = delta
            }
    in
    if transition.previous == transition.next then
        ( model, Cmd.none )

    else
        case onRouteChange transition model of
            ( newModel, Cancel ) ->
                ( newModel
                , undoTransition transition
                )

            ( newModel, Continue ) ->
                ( { newModel | route = transition.next }, Cmd.none )


undoTransition : Transition -> Cmd msg
undoTransition transition =
    let
        log =
            Debug.log "Undoing Transition" (transitionSummary transition)
    in
    moveHistoryBy -transition.delta


redoTransition : Transition -> Cmd msg
redoTransition transition =
    let
        log =
            Debug.log "Redoing Transition" (transitionSummary transition)
    in
    moveHistoryBy transition.delta


transitionSummary : Transition -> String
transitionSummary transition =
    Debug.toString transition.previous.page
        ++ " -> "
        ++ Debug.toString transition.next.page


port pushUrl : String -> Cmd msg


port replaceUrl : String -> Cmd msg


port moveHistoryBy : Int -> Cmd msg


type alias UrlChange =
    { url : String
    , delta : Int
    }


port urlChanged : (UrlChange -> msg) -> Sub msg



-- VIEW


view : Model -> Document Msg
view model =
    { title = titleFor model.route.page
    , body =
        [ div []
            [ promptView model
            , navView
            ]
        , pageView model.route.page
        , appExplanation
        ]
    }


titleFor : Page -> String
titleFor page =
    case page of
        Page1 ->
            "Page 1"

        Page2 ->
            "Page 2"

        Page3 ->
            "Page 3"

        NotFound ->
            "Not Found"


promptView : Model -> Html Msg
promptView model =
    case model.navigationPrompt of
        Prompting prompt transition ->
            div []
                [ text prompt
                , button [ onClick (TransitionConfirmed transition) ] [ text "Yes" ]
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
            This app demonstrates a smarter approach to cancelling page
            transitions, but it requires moving a lot of navigation behavior
            outside of Elm and a lot of bookkeeping of the state of history.
            """ ]
        , p []
            [ text """
            The transition types and handling could use some refinement, but
            the core idea should be sound.
            """
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    urlChanged
        (\change ->
            case Url.fromString change.url of
                Just url ->
                    UrlChangeWithDelta
                        { url = url
                        , delta = change.delta
                        }

                Nothing ->
                    NoOp
        )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , onUrlChange = always NoOp
        , onUrlRequest = UrlRequested
        , subscriptions = subscriptions
        }
