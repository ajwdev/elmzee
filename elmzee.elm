-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Debug
import Random
import Tuple



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  -- TODO Would it be cleaner to put all of these in a two lists? Or at least tuple of (value,lock) ?
  { firstFace: Int
  , firstLock: Bool
  , secondFace: Int
  , secondLock: Bool
  , thirdFace: Int
  , thirdLock: Bool
  , fourthFace: Int
  , fourthLock: Bool
  , fifthFace: Int
  , fifthLock: Bool
  }


init : (Model, Cmd Msg)
init =
  (
    { firstFace = 1
    , firstLock = False
    , secondFace = 2
    , secondLock = False
    , thirdFace = 3
    , thirdLock = False
    , fourthFace = 4
    , fourthLock = False
    , fifthFace = 5
    , fifthLock = False
    }, Cmd.none
  )

-- UPDATE


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


rollDie : Random.Generator Int
rollDie =
  Random.int 1 6


rollAll : Int -> Random.Generator (List Int)
rollAll len =
  Random.list len rollDie


rollable : Model -> List Position
rollable model =
  List.filter (not << lockFromPosition model) positions


valueFromPosition : Model -> Position -> String
valueFromPosition model pos =
  case pos of
    First ->
      toString model.firstFace

    Second ->
      toString model.secondFace

    Third ->
      toString model.thirdFace

    Fourth ->
      toString model.fourthFace

    Fifth ->
      toString model.fifthFace


lockFromPosition : Model -> Position -> Bool
lockFromPosition model pos =
  case pos of
    First ->
      model.firstLock

    Second ->
      model.secondLock

    Third ->
      model.thirdLock

    Fourth ->
      model.fourthLock

    Fifth ->
      model.fifthLock


-- TODO Delete this?
-- toggleLock : Model -> Position -> Model
-- toggleLock model pos =
--   case pos of
--     First ->
--       { model | firstLock = not model.firstLock }

--     Second ->
--       { model | secondLock = not model.secondLock }

--     Third ->
--       { model | thirdLock = not model.thirdLock }

--     Fourth ->
--       { model | fourthLock = not model.fourthLock }

--     Fifth ->
--       { model | fifthLock = not model.fifthLock }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    updateDice : Model -> List (Int, Position) -> Model
    updateDice model values =
      case values of
        [] -> model
        (x::xs) ->
          case Tuple.second x of
            First ->  updateDice { model | firstFace = Tuple.first x } xs
            Second -> updateDice { model | secondFace = Tuple.first x } xs
            Third ->  updateDice { model | thirdFace = Tuple.first x } xs
            Fourth -> updateDice { model | fourthFace = Tuple.first x } xs
            Fifth ->  updateDice { model | fifthFace = Tuple.first x } xs

  in
    case msg of
      Roll ->
        (model, Random.generate UpdateFaces (rollAll (List.length (rollable model))))

      UpdateFaces values ->
        (updateDice model (List.map2 (,) values (rollable model)), Cmd.none)

      ToggleLock pos ->
        case pos of
          First ->  ({ model | firstLock = not model.firstLock }, Cmd.none)
          Second -> ({ model | secondLock = not model.secondLock }, Cmd.none)
          Third ->  ({ model | thirdLock = not model.thirdLock }, Cmd.none)
          Fourth -> ({ model | fourthLock = not model.fourthLock }, Cmd.none)
          Fifth ->  ({ model | fifthLock = not model.fifthLock }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

diceComponent : Model -> Position -> Html Msg
diceComponent model pos =
  div []
    [ h1 [] [ text (valueFromPosition model pos) ]
    , button [ onClick (ToggleLock pos) ] [ text "Lock" ]
    ]


view : Model -> Html Msg
view model =
  div []
    [ diceComponent model First
    , diceComponent model Second
    , diceComponent model Third
    , diceComponent model Fourth
    , diceComponent model Fifth
    , button [ onClick Roll ] [ text "Roll" ]
    ]

