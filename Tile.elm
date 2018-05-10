module Tile exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--This encodes the information for one tile in
--the game of minesweeper, Board.elm will use
--this model to construct a game board.

type State = Searched | Unsearched
type Tval = Mine | NumWarning Int

type alias Tile =
  {
  state : State,
  tval : Tval
  }

--Set Functions for Tile model
empty: Tile
empty = { state = Unsearched , tval = NumWarning 0}

value: Int -> Tile
value x = { state = Unsearched , tval = NumWarning x}

mine: Tile
mine ={ state = Unsearched, tval = Mine}

--OnClick we have searched tile
--User doesn not have the Unsearch ability
type Msg = Search

--Return true if tile is a Mine
isMine: Tile -> Bool
isMine {state , tval} =
  case tval of
    Mine -> True
    _ -> False

--Return true of tile tval is Just 0
isZero: Tile -> Bool
isZero {state, tval} =
  case tval of
    NumWarning 0 -> True
    _ -> False

--Set tile state to Unsearched
isUnsearched: Tile -> Bool
isUnsearched {state , tval} =
  case state of
    Unsearched -> True
    Searched -> False

--Set tile state to Searched
search: Tile -> Tile
search {state , tval} = Tile Searched tval

--Basic button style
--Eventually want to had images
myStyle: String -> Attribute msg
myStyle color =
              style
                [ ("width" , "25px"),
                  ("height" , "25px"),
                  ("background-color", color ),
                  ("vertical-align", "top")
                ]

mineStyle: Attribute Msg
mineStyle =
                style
                  [ ("width" , "25px"),
                    ("height" , "25px"),
                    ("background-color", "red"),
                    ("vertical-align", "top")
                  ]

--If button is clicked we run a search on tile
update: Msg -> Tile -> Tile
update msg ({state , tval} as tile) =
  case state of
    --On Searched tile nothing happens
    Searched -> tile
    --On Unsearched tile we set to Searched
    --tval will be revealed in view as result
    Unsearched -> Tile Searched tval

view: Tile -> Html Msg
view ({state , tval} as tile) =
  case state of
    --If tile is searched we have to then look at content
    Searched ->
      case tval of
        Mine ->
            button [mineStyle][]
        NumWarning x ->
            button [myStyle "white"][Html.text (toString x)]
    --If tile is searched show unsearched style
    Unsearched ->
            button [onClick Search, myStyle "grey"][]

main : Program Never Tile Msg
main =
    beginnerProgram
        { model = mine
        , view = view
        , update = update
        }
