{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Control.FRPNow.GTK.MissingFFI
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

Missing but necessary 'Attr' definitions which have not nade it into gtk3hs yet.

-}

module Control.FRPNow.GTK.MissingFFI where

import Graphics.UI.Gtk
import Control.Monad
import qualified Data.Text as T
import System.Glib.GObject
import System.Glib.Attributes
import System.Glib.Properties

scrolledWindowOverlay :: ScrolledWindowClass self => Attr self Bool
scrolledWindowOverlay = newAttrFromBoolProperty "overlay-scrolling"

panedWideHandle :: PanedClass self => Attr self Bool
panedWideHandle = newAttrFromBoolProperty "wide-handle"

progressBarShowText :: ProgressBarClass self => Attr self Bool
progressBarShowText = newAttrFromBoolProperty "show-text"
