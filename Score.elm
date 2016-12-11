module Score exposing
  ( calcThreeOfAKind, calcFourOfAKind
  , smallStraight, largeStraight
  , yahtzee, chance, fullHouse
  , ones, twos, threes, fours, fives, sixes
  )

import Debug
import Set exposing (Set)
import Array exposing (Array)

unwrapInt : Maybe Int -> Int
unwrapInt x =
  case x of
    Nothing ->
      0
    Just val ->
      val


countDuplicates : List Int -> Int
countDuplicates dice =
    unwrapInt (List.maximum (groupBy dice))


groupBy : List Int -> List Int
groupBy dice =
  let
    incr : Int -> Array Int -> Array Int
    incr idx memo =
      Array.set (idx - 1) ((unwrapInt (Array.get (idx - 1) memo)) + 1) memo

    bucket : List Int -> Array Int -> Array Int
    bucket dice memo =
      case dice of
        [] ->
          memo
        x::xs ->
          bucket xs (incr x memo)
  in
    Array.toList
      (bucket dice (Array.fromList [0,0,0,0,0]))


countStraight : List (List Int) -> List Int -> Maybe Int
countStraight combos dice =
  let
    isStraight : List Int -> List Int -> Int
    isStraight dice str =
      Set.size (Set.intersect (Set.fromList str) (Set.fromList dice))
  in
    List.maximum
      (List.map (isStraight dice) combos)


uppers : Int -> List Int -> Maybe Int
uppers len dice =
  Just (List.sum (List.filter (\x -> x == len) dice))



-- Lower Section

calcThreeOfAKind : List Int -> Maybe Int
calcThreeOfAKind dice =
  if (countDuplicates dice >= 3) then
     Just (List.sum dice)
   else
     Nothing

calcFourOfAKind : List Int -> Maybe Int
calcFourOfAKind dice =
  if (countDuplicates dice >= 4) then
     Just (List.sum dice)
   else
     Nothing

yahtzee : List Int -> Maybe Int
yahtzee dice =
  if (countDuplicates dice == 5) then
     Just 50
   else
     Nothing

smallStraight : List Int -> Maybe Int
smallStraight dice =
  if (unwrapInt (countStraight [[1,2,3,4],[2,3,4,5],[3,4,5,6]] dice) >= 4) then
     Just 30
   else
     Nothing

largeStraight : List Int -> Maybe Int
largeStraight dice =
  if (unwrapInt (countStraight [[1,2,3,4,5],[2,3,4,5,6]] dice) == 5) then
     Just 40
   else
     Nothing

fullHouse : List Int -> Maybe Int
fullHouse dice =
  let
      threeOrTwo : List Int -> List Int
      threeOrTwo dice =
        List.filter (\x -> x == 3 || x == 2) (groupBy dice)
  in
    if ((List.length (threeOrTwo dice)) == 2) then
       Just 25
     else
       Nothing

chance : List Int -> Maybe Int
chance dice =
  Just (List.sum dice)


 -- Upper section

ones : List Int -> Maybe Int
ones dice =
  uppers 1 dice

twos : List Int -> Maybe Int
twos dice =
  uppers 2 dice

threes : List Int -> Maybe Int
threes dice =
  uppers 3 dice

fours : List Int -> Maybe Int
fours dice =
  uppers 4 dice

fives : List Int -> Maybe Int
fives dice =
  uppers 5 dice

sixes : List Int -> Maybe Int
sixes dice =
  uppers 6 dice
