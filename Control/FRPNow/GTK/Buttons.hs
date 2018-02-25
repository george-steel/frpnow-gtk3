{- |
Module      :  Control.FRPNow.GTK.Buttons
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

Functions for creating buttons which interact with FRPNow.

-}

module Control.FRPNow.GTK.Buttons (
    IconName, createButton, createDynamicButton, createToggleButton,
    -- * Checkboxes
    createCheckButton, createStaticChecklist, createDynamicChecklist,
) where

import Control.FRPNow.GTK.Core
import Control.FRPNow.GTK.Containers
import Graphics.UI.Gtk
import Control.Applicative
import Control.Monad
import Control.FRPNow
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

-- | Type for standard icon names.
-- The value should be contained in the standard list at <https://developer.gnome.org/icon-naming-spec/>.
type IconName = T.Text

-- | Creates a button with (optionally) text and an icon. Returns the button and when it is pressed.
createButton :: Maybe IconName -> Maybe Text -> Now (Button, EvStream ())
createButton micon mlbl = do
    btn <- sync buttonNew
    iattr <- case micon of
        Just icon -> do
            img <- sync $ imageNewFromIconName icon IconSizeButton
            return [buttonImage := img]
        Nothing -> return []
    let tattr = maybeToList (fmap (buttonLabel :=) mlbl)
    sync $ set btn (iattr ++ tattr)
    pressed <- getUnitSignal buttonActivated btn
    return (btn, pressed)

-- | Creates a toggle button with an initial state. Returns the button and its current state.
createToggleButton :: Maybe IconName -> Maybe Text -> Bool -> Now (ToggleButton, Behavior Bool)
createToggleButton micon mlbl initstate = do
    btn <- sync toggleButtonNew
    iattr <- case micon of
        Just icon -> do
            img <- sync $ imageNewFromIconName icon IconSizeButton
            return [buttonImage := img]
        Nothing -> return []
    let tattr = maybeToList (fmap (buttonLabel :=) mlbl)
    sync $ set btn (iattr ++ tattr ++ [toggleButtonActive := initstate])
    updated <- getSignal toggled btn (toggleButtonGetActive btn >>=)
    st <- sample $ fromChanges initstate updated
    return (btn,st)

-- | Creates a button with dynamic text.
createDynamicButton :: Behavior Text -> Now (Button,EvStream ())
createDynamicButton s = do
    button <- sync buttonNew
    setAttr buttonLabel button s
    stream <- getUnitSignal buttonActivated button
    return (button,stream)

--------------------------------------------------------------------------------

-- | Creates a checkbox with text an an initial state. Returns the widget and its current state
createCheckButton :: Text -> Bool -> Now (CheckButton, Behavior Bool)
createCheckButton txt initstate = do
    btn <- sync $ checkButtonNewWithLabel txt
    sync $ set btn [toggleButtonActive := initstate]
    updated <- getSignal toggled btn (toggleButtonGetActive btn >>=)
    st <- sample $ fromChanges initstate updated
    return (btn,st)


createChecklistItem :: (a, Text) -> Bool -> Now (CheckButton, Behavior [a])
createChecklistItem (val, txt) initstate = do
    (btn,st) <- createCheckButton txt initstate
    return (btn, fmap (\b -> if b then [val] else []) st)

-- | Creates a set of checkboxes from a list of (item,label) pairs and a list of initially-checked items. Returns a list of 'CheckButton's (use a function on the "Containers" module to pack them) and the currently-selected items.
createStaticChecklist :: Eq a => [(a,Text)] -> [a] -> Now ([CheckButton], Behavior [a])
createStaticChecklist items startchecked = do
    btns <- forM items $ \item@(val,txt) ->
        createChecklistItem item (val `elem` startchecked)
    let (cbs, results) = unzip btns
        vals = mconcat results
    return (cbs, vals)

-- | Creates a checklist to select from a dynamic list of objects (updating the displayed checkboxes). Returns the checklist (stacked vertically in a VBox) and the currently selected items.
createDynamicChecklist :: Eq a => Behavior [(a, Text)] -> Now (VBox, Behavior [a])
createDynamicChecklist dynitems = do
    box <- sync $ vBoxNew False 0
    inititems <- sample dynitems
    (initboxes,initselected) <- createStaticChecklist inititems []
    sync $ forM_ initboxes $ \cb -> boxPackStart box cb PackNatural 0

    let itemsChanged = toChanges dynitems
    (outReplaced,replaceOut) <- callbackStream
    selected <- sample $ foldrSwitch initselected outReplaced

    flip callStream itemsChanged $ \itemslist -> do
        let newitems = last itemslist
        oldselection <- sample selected
        sync $ clearChildren box
        (newboxes,newselected) <- createStaticChecklist newitems oldselection
        sync $ forM_ newboxes $ \cb -> boxPackStart box cb PackNatural 0
        sync $ widgetShowAll box
        sync $ replaceOut newselected
        return ()

    return (box,selected)
