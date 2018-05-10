module GameBoard exposing (..)

import Tile exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import Random as R exposing (..)
import Time

--Coordinates of a tile on board (Rows x Columns)
-- type alias Coordinates = (Int , Int)
--Board is a 2d array of Tiles
type alias Board = Matrix Tile

--Run search on Coordinates of click tile
--And send the Tile.elm msg to Tile.elm
type Msg = Search Matrix.Location Tile.Msg | Restart

--Classic Minsweeper on Expert Difficulty
-- Columns 16
-- Rows 30
-- 30 x 16 Board
-- Mines = 130
numberOfColumns = 30
numberOfRows = 16
numberOfMines = 130

init: Board
init = addWarnings(addMines(emptyBoard))

restart: Board -> Board
restart board =
  addWarnings(addMines(emptyBoard))

emptyBoard: Board
emptyBoard = matrix numberOfRows numberOfColumns (\location -> Tile.empty)

getTile: Matrix.Location -> Board -> Maybe Tile
getTile (x, y) board =
  get (loc x y) board

--The passed Coordinates is the clicked tiles
adjacentTiles: Matrix.Location -> Board -> List Tile
adjacentTiles location board =
  let (x , y) = location in
  List.filterMap (\loc -> getTile loc board ) ([(x - 1,y-1)
  ,(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)])


isTile: Maybe (Matrix Tile) -> Matrix Tile
isTile x =
  case x of
    Just y -> y
    Nothing -> Debug.crash "error: isRow Nothing"

gameover: Board -> Board
gameover board=
  Matrix.map (\tile -> if Tile.isMine tile then Tile.search tile else tile) board

searchTile: Location -> Board -> Board
searchTile (x,y) board =
  let tile = getTile (x,y) board in
  case tile of
    Just tile ->
      if Tile.isUnsearched tile then
        if Tile.isMine tile then
          gameover board
        else
          set (loc x y) (Tile.search tile) board
      else
        board
    Nothing -> board

randomLocGen: Int -> List Location
randomLocGen seed =
  let (result, newseed) = R.step (R.list numberOfMines (R.pair (R.int 0 (numberOfRows-1)) (R.int 0 (numberOfColumns-1)))) (R.initialSeed seed)
  in
     result


setMines: List Location -> Board -> Board
setMines list board =
  List.foldl (\(x,y) -> Matrix.set (loc x y) (Tile Unsearched Mine)) board list


addMines: Board -> Board
addMines board =
  setMines (randomLocGen 321 ) board


sumAdjacent: Location -> Tile -> Board -> Tile
sumAdjacent location tile board =
  if Tile.isMine tile then
    tile
  else
    let listTiles = adjacentTiles location board in
    let sum1 = (List.foldl (\tile sum ->
      if Tile.isMine tile then
        sum + 1
      else
        sum) 0 listTiles) in
    Tile.value sum1


addWarnings: Board -> Board
addWarnings board =
  Matrix.mapWithLocation (\location tile -> sumAdjacent location tile board) board



update: Msg -> Board -> Board
update msg board =
  case msg of
    Search coordinates tileAction ->
      searchTile coordinates board
    Restart ->
      restart board 



view: Board ->  Html Msg
view board =
    let
      l location tile = Html.map (Search (Matrix.row location , Matrix.col location))(Tile.view tile)
    in
     div [] [img [ src "/logo.png", alt "logo" , width 750 , height 200 ] [] ,
     div[] (List.map (div []) (Matrix.toList(mapWithLocation l board))),
     div [] [button [onClick Restart] [text("restart")]]
     ]

main : Program Never Board Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }
