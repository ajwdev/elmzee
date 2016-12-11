import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Random
import Tuple
import Debug

import Score

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
  , round: Int
  , turn: Int
  , lower: Array (Maybe Int)
  , upper: Array (Maybe Int)
  }


initDie : (Int, Bool)
initDie =
  (1, False)


init : (Model, Cmd Msg)
init =
  (
    { dice = Array.initialize 5 (always initDie)
    , round = 0
    , turn = 0
    , lower = Array.initialize (List.length lowerScores) (always Nothing)
    , upper = Array.initialize (List.length upperScores) (always Nothing)
    }
    , Cmd.none
  )


type Msg
  = Roll
  | ToggleLock Position
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


type Upper
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives

type Lower
  = ThreeOfAKind
  | FourOfAKind
  | FullHouse
  | SmallStraight
  | LargeStraight
  | Yahtzee
  | Chance

upperScores =
  [ Ones
  , Twos
  , Threes
  , Fours
  , Fives
  ]

lowerScores =
  [ ThreeOfAKind
  , FourOfAKind
  , FullHouse
  , SmallStraight
  , LargeStraight
  , Yahtzee
  , Chance
  ]


maybeSum : Maybe Int -> Maybe Int -> Maybe Int
maybeSum a b =
  Just ((Maybe.withDefault 0 a) + (Maybe.withDefault 0 b))

calcLower : Model -> Int
calcLower model =
  case Array.foldl maybeSum (Just 0) model.lower of
    Nothing ->
      0

    Just sum ->
      sum

calcUpper : Model -> Int
calcUpper model =
  case Array.foldl maybeSum (Just 0) model.upper of
    Nothing ->
      0

    Just sum ->
      sum

score : Model -> Int
score model =
  (+) (calcLower model) (calcUpper model)


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


getAllValues : Model -> List Int
getAllValues model =
  Array.toList (Array.map Tuple.first model.dice)


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
            Nothing ->
              (-1, False) -- This shouldn't happen

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

unwrapString : Maybe a -> String
unwrapString a =
  case a of
    Nothing ->
      "0"
    Just val ->
      toString val


diceComponent : Model -> Position -> Html Msg
diceComponent model pos =
  let
    buttonDisplay : Model -> Position -> String
    buttonDisplay model pos =
      if (getLock model pos) then "Unlock" else "Lock"

  in
    div [ class "fixed" ]
      [ h1 [ class "dice" ] [ text (toString (getValue model pos)) ]
      , button
        [ onClick (ToggleLock pos)
        , class "lock-button"
        ] [ text (buttonDisplay model pos) ]
      ]


scoreboardComponent : Model -> Html Msg
scoreboardComponent model =
  div [ class "scoreboard" ]
  [ div [] [ text ("Three of a kind: " ++ (unwrapString (Score.calcThreeOfAKind (getAllValues model)))) ]
  , div [] [ text ("Four of a kind: " ++ (unwrapString (Score.calcFourOfAKind (getAllValues model)))) ]
  , div [] [ text ("Yahtzee: " ++ (unwrapString (Score.yahtzee (getAllValues model)))) ]
  , div [] [ text ("Small straight: " ++ (unwrapString (Score.smallStraight (getAllValues model)))) ]
  , div [] [ text ("Large straight: " ++ (unwrapString (Score.largeStraight (getAllValues model)))) ]
  , div [] [ text ("Full House: " ++ (unwrapString (Score.fullHouse (getAllValues model)))) ]
  , div [] [ text ("Chance: " ++ (unwrapString (Score.chance (getAllValues model)))) ]
  , div [] [ text ("Ones: " ++ (unwrapString (Score.ones (getAllValues model)))) ]
  , div [] [ text ("Twos: " ++ (unwrapString (Score.twos (getAllValues model)))) ]
  , div [] [ text ("Threes: " ++ (unwrapString (Score.threes (getAllValues model)))) ]
  , div [] [ text ("Fours: " ++ (unwrapString (Score.fours (getAllValues model)))) ]
  , div [] [ text ("Fives: " ++ (unwrapString (Score.fives (getAllValues model)))) ]
  , div [] [ text ("Sixes: " ++ (unwrapString (Score.sixes (getAllValues model)))) ]
  ]


view : Model -> Html Msg
view model =
  div [ class "container", style [("width", "400px")] ]
  [ h1 [ ] [ text (toString (score model)) ]
  , div [ class "container-row" ]
    [ diceComponent model First
    , diceComponent model Second
    , diceComponent model Third
    , diceComponent model Fourth
    , diceComponent model Fifth
    ]
  , div [ class "container-row" ]
    [ button
      [ onClick Roll
      , style
        [ ("width", "400px")
        , ("height","40px")
        , ("margin-top", "10px")
        ]
      ] [ text "Roll" ]
    ]
  , div [ class "container-row" ] [ scoreboardComponent model ]
  ]
  |> withStyle

