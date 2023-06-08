{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import GI.Gtk hiding (Widget,Bin,(:=),Container,on,main)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State = NotClicked | Clicked

data Event = ButtonClicked | Close

update' :: State -> Event -> Transition State Event
update' _ Close = Exit
update' _ ButtonClicked = Transition Clicked (return Nothing)

drawWindow w s = bin Window
    [ #title := "Hello World" ]
    , on #deleteEvent (const (True,Close))
    ] $ w s

drawBody :: State -> Widget Event
drawBody Clicked = widget Button [ #label := "Thank you!" ]
drawBody NotClicked = widget Button [ #label := "Click Me!"
                                    , on #clicked ButtonClicked
                                    ]


main :: IO ()
main = void $ run App { view = drawWindow drawBody
                      , update = update'
                      , inputs = []
                      , initialState = NotClicked
                      }
