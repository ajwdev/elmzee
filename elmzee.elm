-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Debug
import Random
import Tuple
import Array exposing (Array)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Die =
  (Int, Bool)


type alias Model =
  { dice: Array Die
  }


initDie : (Int, Bool)
initDie =
  (1, False)


init : (Model, Cmd Msg)
init =
  (
    { dice = Array.initialize 5 (always initDie)
    }
    , Cmd.none
  )


type Msg
  = Roll
  | ToggleLock Position
  -- | NewFace (Position Int)
  | UpdateFaces (List Int)


type Position
  = First
  | Second
  | Third
  | Fourth
  | Fifth


positions =
  [ First
  , Second
  , Third
  , Fourth
  , Fifth
  ]


indexFromPosition : Position -> Int
indexFromPosition pos =
  case pos of
    First  -> 0
    Second -> 1
    Third  -> 2
    Fourth -> 3
    Fifth  -> 4


rollDie : Random.Generator Int
rollDie =
  Random.int 1 6


rollAll : Int -> Random.Generator (List Int)
rollAll len =
  Random.list len rollDie


rollable : Model -> List Position
rollable model =
  List.filter (not << getLock model) positions


getValue : Model -> Position -> Int
getValue model pos =
  case Array.get (indexFromPosition pos) model.dice of
    Nothing ->
      -1

    Just value ->
      Tuple.first value


getLock : Model -> Position -> Bool
getLock model pos =
  case Array.get (indexFromPosition pos) model.dice of
    Nothing ->
      False

    Just value ->
      Tuple.second value


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    updateSingleValue : Position -> Int -> Array Die -> Array Die
    updateSingleValue pos value dice =
      let
        setVal : Maybe Die -> Int -> Die
        setVal dice val =
          case dice of
            Nothing -> (-1, False) -- This shouldn't happen

            Just x ->
              Tuple.mapFirst (\_ -> val) x
      in

      Array.set
        (indexFromPosition pos)
        (setVal (Array.get (indexFromPosition pos) dice) value)
        dice


    toggleLock: Position -> Array Die -> Array Die
    toggleLock pos dice =
      let
        setLock : Maybe Die -> Die
        setLock dice =
          case dice of
            Nothing ->
              (-1, False) -- This shouldn't happen

            Just x ->
              Tuple.mapSecond not x
      in
      Array.set
        (indexFromPosition pos)
        (setLock (Array.get (indexFromPosition pos) dice))
        dice


    updateValue : List (Position, Int) -> Model -> Model
    updateValue values model =
      case values of
        [] -> model
        (x::xs) ->
          updateValue xs {
            model | dice = updateSingleValue (Tuple.first x) (Tuple.second x) model.dice
          }

  in
    case msg of
      Roll ->
        (model, Random.generate UpdateFaces (rollAll (List.length (rollable model))))

      UpdateFaces values ->
        Debug.log (toString values)
        (updateValue (List.map2 (,) (rollable model) values) model, Cmd.none)

      ToggleLock pos ->
        ({ model | dice = toggleLock pos model.dice }, Cmd.none)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
withStyle html =
  div []
  [ node "style" [type_ "text/css"]
    [text "@import url(/css/elmzee.css)"]
  , html
  ]

diceComponent : Model -> Position -> Html Msg
diceComponent model pos =
  let
    buttonDisplay : Model -> Position -> String
    buttonDisplay model pos =
      if (getLock model pos) then "Unlock" else "Lock"

  in
    div [ class "fixed" ]
      [ h1 [ class "dice" ] [ text (toString (getValue model pos)) ]
      , button [
          onClick (ToggleLock pos),
          class "lock-button"
        ] [ text (buttonDisplay model pos) ]
      ]


view : Model -> Html Msg
view model =
  div [ class "container", style [("width", "400px")] ]
    [ div [ class "container-row" ]
      [ diceComponent model First
      , diceComponent model Second
      , diceComponent model Third
      , diceComponent model Fourth
      , diceComponent model Fifth
      ],
      div [ class "container-row" ]
      [ button [ onClick Roll, style [("width", "400px"),("height","40px")] ] [ text "Roll" ]
      ]
    ]
  |> withStyle

