import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Platform.Cmd exposing (Cmd)
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
  | UpdateScore Score


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


type Score
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | ThreeOfAKind
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
  , Sixes
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


indexOf : a -> Array a -> Int
indexOf x xs =
  case (List.head (List.filter
         (\t -> (Tuple.second t) == x) (Array.toIndexedList xs))) of
    Just t ->
      Tuple.first t

    Nothing ->
      -1

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


clearLocks: Array Die -> Array Die
clearLocks dice =
  Array.map (Tuple.mapSecond (\_ -> False)) dice

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


upperFunc : Score -> (List Int -> Maybe Int)
upperFunc score =
  case score of
    Ones -> Score.ones
    Twos -> Score.twos
    Threes -> Score.threes
    Fours -> Score.fours
    Fives -> Score.fives
    Sixes -> Score.sixes
    _ -> Debug.crash "Should not be here"

lowerFunc : Score -> (List Int -> Maybe Int)
lowerFunc score =
  case score of
    FullHouse -> Score.fullHouse
    ThreeOfAKind -> Score.calcThreeOfAKind
    FourOfAKind -> Score.calcFourOfAKind
    SmallStraight -> Score.smallStraight
    LargeStraight -> Score.largeStraight
    Yahtzee -> Score.yahtzee
    Chance -> Score.chance
    _ -> Debug.crash "Should not be here"

setScore : Score -> Model -> Model
setScore score model =
  let
    unwrapInt : Maybe Int -> Int
    unwrapInt x =
      case x of
        Nothing ->
          0
        Just val ->
          val

    indexUpper : Score -> Int
    indexUpper score =
      indexOf score (Array.fromList upperScores)

    indexLower : Score -> Int
    indexLower score =
      indexOf score (Array.fromList lowerScores)

    updateStoreCache : Maybe Int -> Int -> Array (Maybe Int) -> Array (Maybe Int)
    updateStoreCache score idx scoreArr =
      Array.set idx (score) scoreArr

  in
    if (List.member score upperScores) then
      { model | upper = updateStoreCache
        ((upperFunc score) (getAllValues model)) (indexUpper score) model.upper
      , turn = 0
      , round = model.round + 1
      , dice = clearLocks model.dice
      }
    else
      { model | lower = updateStoreCache
        ((lowerFunc score) (getAllValues model)) (indexLower score) model.lower
      , turn = 0
      , round = model.round + 1
      , dice = clearLocks model.dice
      }


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
        ({model | turn = model.turn + 1},
           Random.generate UpdateFaces (rollAll (List.length (rollable model)))
        )

      UpdateFaces values ->
        (updateValue (List.map2 (,) (rollable model) values) model, Cmd.none)

      ToggleLock pos ->
        ({ model | dice = toggleLock pos model.dice }, Cmd.none)

      UpdateScore score ->
        (setScore score model, Cmd.none)



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
  let
      scoreText : String -> (List Int -> Maybe Int) -> Model -> String
      scoreText desc scorer dice =
        desc ++ ": " ++ (unwrapString (scorer (getAllValues dice)))

      scoreOnClick : Score -> Attribute Msg
      scoreOnClick score =
        onClick (UpdateScore score)
  in
    div [ class "scoreboard" ]
    [ button [ scoreOnClick ThreeOfAKind ]
        [ text (scoreText "Three of a Kind" (lowerFunc ThreeOfAKind) model) ]
    , button [ scoreOnClick FourOfAKind ]
         [ text (scoreText "Four of a Kind" (lowerFunc FourOfAKind) model) ]
    , button [ scoreOnClick Yahtzee ]
         [ text (scoreText "Yahtzee" (lowerFunc Yahtzee) model) ]
    , button [ scoreOnClick SmallStraight ]
         [ text (scoreText "Small Straight" (lowerFunc SmallStraight) model) ]
    , button [ scoreOnClick LargeStraight ]
         [ text (scoreText "Large Straight" (lowerFunc LargeStraight) model) ]
    , button [ scoreOnClick FullHouse ]
         [ text (scoreText "Full House" (lowerFunc FullHouse) model) ]
    , button [ scoreOnClick Chance ]
         [ text (scoreText "Chance" (lowerFunc Chance) model) ]
    , button [ scoreOnClick Ones ]
         [ text (scoreText "Ones" (upperFunc Ones) model) ]
    , button [ scoreOnClick Twos ]
         [ text (scoreText "Twos" (upperFunc Twos) model) ]
    , button [ scoreOnClick Threes ]
         [ text (scoreText "Threes" (upperFunc Threes) model) ]
    , button [ scoreOnClick Fours ]
         [ text (scoreText "Fours" (upperFunc Fours) model) ]
    , button [ scoreOnClick Fives ]
         [ text (scoreText "Fives" (upperFunc Fives) model) ]
    , button [ scoreOnClick Sixes ]
         [ text (scoreText "Sixes" (upperFunc Sixes) model) ]
    ]

rollComponent : Model -> Html Msg
rollComponent model =
  button
  [ onClick Roll
  , disabled (if model.turn > 2 then True else False)
  , style
    [ ("width", "400px")
    , ("height","40px")
    , ("margin-top", "10px")
    ]
  ] [ text "Roll" ]


view : Model -> Html Msg
view model =
  div [ class "container", style [("width", "400px")] ]
  [ h1 [ ] [ text (toString (score model)) ]
  , h1 [ ] [ text (toString (model.turn)) ]
  , div [ class "container-row" ]
    [ diceComponent model First
    , diceComponent model Second
    , diceComponent model Third
    , diceComponent model Fourth
    , diceComponent model Fifth
    ]
  , div [ class "container-row" ] [ rollComponent model ]
  , div [ class "container-row" ] [ scoreboardComponent model ]
  ]
  |> withStyle

