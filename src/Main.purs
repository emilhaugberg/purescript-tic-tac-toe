module Main where

import Prelude (class Show, Unit, bind, show, const, map, (<<<), (==), (<>))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, div, text, button)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)

import Data.Tuple (Tuple(..), snd, fst)
import Data.Array (range)

data Action = Click Int
            | Reset

type Position = Int
data Token = X | O | E
type Board = Array (Tuple Position Token)

type State = { activePlayer :: Token, board :: Board }

initState :: State
initState = { activePlayer: X, board: map (\i -> Tuple i E) (range 1 9) }

update :: Action -> State -> State
update (Click i) st = { activePlayer: updatePlayer st.activePlayer, board: updateBoard i st }
update Reset st = initState

updatePlayer :: Token -> Token
updatePlayer tk =
  case tk of
    X -> O
    O -> X
    E -> E

updateBoard :: Int -> State -> Board
updateBoard pos st =
  map f st.board
  where
    f tup = if fst tup == pos
               then g tup
               else tup
    g tup =
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
    mkBlock tup = div [className ("block " <> (show <<< snd) tup), onClick (const (Click (fst tup)))] [(text <<< show) (snd tup)]

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: initState
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
