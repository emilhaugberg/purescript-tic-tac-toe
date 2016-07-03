module Main where

import Prelude (class Show, Unit, bind, show, const, map, (<<<), (==), (<>), (-))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, div, text, button)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)

import Data.Tuple (Tuple(..), snd, fst)
import Data.Array (range, index)
import Data.Maybe

data Action = Click Int
            | Reset

type Position = Int
data Token = X | O | E
type Board = Array (Tuple Position Token)

type State = { activePlayer :: Token, board :: Board }

initState :: State
initState = { activePlayer: X, board: map (\i -> Tuple i E) (range 1 9) }

-- update :: Action -> State -> State
-- update (Click i) st = { activePlayer: updatePlayer st.activePlayer, board: updateBoard i st }
-- update Reset st = initState

update :: Action -> State -> State
update (Click i) st = updateState i st
update Reset st = initState

updateState :: Position -> State  -> State
updateState pos st =
  case index st.board (pos - 1) of
    Just (Tuple pos X) -> st
    Just (Tuple pos O) -> st
    Just (Tuple pos E) -> { activePlayer: updatePlayer st.activePlayer, board: updateBoard pos st }
    Nothing -> st

updatePlayer :: Token -> Token
updatePlayer tk =
  case tk of
    X -> O
    O -> X
    E -> E

updateBoard :: Position -> State -> Board
updateBoard pos st =
  map f st.board
  where
    f :: Tuple Position Token -> Tuple Position Token
    f tup = if fst tup == pos
               then checkIfAlreadyClicked tup
               else tup

    checkIfAlreadyClicked :: Tuple Position Token -> Tuple Position Token
    checkIfAlreadyClicked tup =
      case snd tup of
        X -> tup
        O -> tup
        E -> Tuple pos st.activePlayer

instance showToken :: Show Token where
  show X = "X"
  show O = "O"
  show E = "E"

view :: State -> Html Action
view gmState =
 div []
  [ div [className "board"] blocks
  , div [] [button [onClick (const Reset)] [text "restart"]]
  ]

  where
    blocks :: Array (Html Action)
    blocks = map mkBlock gmState.board

    mkBlock :: Tuple Int Token -> Html Action
    mkBlock tup = div [className ("block " <> (show <<< snd) tup <> " " <> (show <<< fst) tup), onClick (const (Click (fst tup)))] [(text <<< show) (snd tup)]

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: initState
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
