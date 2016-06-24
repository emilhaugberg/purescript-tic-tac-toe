module Board where

import Pux.Html (Html, div)
import Pux.Html.Attributes (className)

board :: forall a. Html a
board =
  div [className "board"] [
    boardRow,
    boardRow,
    boardRow
  ]


boardRow :: forall a. Html a
boardRow =
  div [className "row"]
      [ div [className "block"] []
      , div [className "block"] []
      , div [className "block"] []
      ]
