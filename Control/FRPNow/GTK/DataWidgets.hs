{- |
Module      :  Control.FRPNow.GTK.DataWidgets
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

Functions for creating widgets allowing entry and display of variable data, reperesented as 'Behavior's
-}

module Control.FRPNow.GTK.DataWidgets where

import Control.FRPNow.GTK.Core
import Control.FRPNow.GTK.MissingFFI
import Graphics.UI.Gtk
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.FRPNow
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

-- | Version of 'show' which outputs 'Text'
showtext :: (Show a) => a -> Text
showtext = T.pack . show

-- * Labels

-- | Creates a label containing static text
createLabel :: (MonadIO m) => Text -> m Label
createLabel t = liftIO $ do
    l <- labelNew (Just t)
    set l [miscXalign := 0]
    return l

-- | Creates a label containing dynamic text
createLabelDisplay :: Behavior Text -> Now Label
createLabelDisplay s = do
     l <- sync $ labelNew (Nothing :: Maybe Text)
     sync $ set l [miscXalign := 0, labelWrap := True]
     setAttr labelLabel l s
     return l

-- | Creates a TextView with editing disabled containing dynamic text. Useful for showing text too long for a Label.
createTextViewDisplay :: Behavior Text -> Now TextView
createTextViewDisplay dyntext = do
    tv <- sync textViewNew
    done <- getUnrealize tv
    buf <- sync $ textBufferNew Nothing
    inittext <- sample dyntext
    let updates = toChanges dyntext `beforeEs` done
    sync $ set buf [textBufferText := inittext]
    callIOStream (\x -> set buf [textBufferText := x]) updates
    sync $ textViewSetBuffer tv buf
    return tv

-- * Entry

-- | Creates an Entey with an initial value
createEntry :: String -> Now (Entry, Behavior String)
createEntry inittext = do
    entry <- sync $ entryNew
    sync $ set entry [entryText := inittext]
    edits <- getSignal editableChanged entry (entryGetText entry >>=)
    btext <- sample $ fromChanges inittext edits
    return (entry, btext)

-- | Creates an Entry which only allows characters satisfying as predicate. Useful for numeric entry.
createFilteredEntry :: (Char -> Bool) -> String -> Now (Entry, Behavior String)
createFilteredEntry f inittext = do
    entry <- sync $ entryNew
    sync $ set entry [entryText := filter f inittext]
    sync . mfix $ \cid ->
        entry `on` insertText $ \str pos -> do
            signalBlock cid
            pos' <- editableInsertText entry (filter f str) pos
            signalUnblock cid
            stopInsertText cid
            return pos'
    edits <- getSignal editableChanged entry (entryGetText entry >>=)
    btext <- sample $ fromChanges inittext edits
    return (entry, btext)

-- | Creates a floating-point SpinButton with range, step size, and initial value.
createSpinEntry :: (Double, Double) -> Double -> Double -> Now (SpinButton, Behavior Double)
createSpinEntry (a,b) stepsize initval = do
    sbtn <- sync $ spinButtonNewWithRange a b stepsize
    sync $ spinButtonSetValue sbtn initval
    (changes, changecb) <- callbackStream
    sync $ onValueSpinned sbtn (changecb =<< spinButtonGetValue sbtn)
    val <- sample $ fromChanges initval changes
    return (sbtn,val)

-- | Creates a integer SpinButton with range, step size, and initial value.
createIntSpinEntry :: (Int, Int) -> Int -> Int -> Now (SpinButton, Behavior Int)
createIntSpinEntry (a,b) stepsize initval = do
    sbtn <- sync $ spinButtonNewWithRange (fromIntegral a) (fromIntegral b) (fromIntegral stepsize)
    sync $ spinButtonSetValue sbtn (fromIntegral initval)
    (changes, changecb) <- callbackStream
    sync $ onValueSpinned sbtn (changecb =<< spinButtonGetValueAsInt sbtn)
    val <- sample $ fromChanges initval changes
    return (sbtn,val)

-- * Progress

-- | Creates a progress bar which displays an annotation and can optionally be disabled.
createProgressBar :: Behavior (Maybe (Double, Text)) -> Now ProgressBar
createProgressBar mprogress = do
    let isactive = fmap isJust mprogress
        position = fmap (maybe 0 fst) mprogress
        lbl = fmap (maybe T.empty snd) mprogress
    bar <- sync progressBarNew
    sync $ set bar [progressBarShowText := True]
    setAttr progressBarFraction bar position
    setAttr progressBarText bar lbl
    setAttr widgetSensitive bar isactive
    return bar

-- | Creates a progress bar which is always on and displays the progres value only.
createSimpleProgressBar :: Behavior Double -> Now ProgressBar
createSimpleProgressBar progress = do
    bar <- sync $ progressBarNew
    setAttr progressBarFraction bar progress
    return bar

-- | Creates a spinner which spins when its parameter is True.
createSpinner :: Behavior Bool -> Now Spinner
createSpinner b = do
    spn <- sync $ spinnerNew
    setAttr spinnerActive spn b
    return spn

-- * Sliders

-- | Creates a slider with range, step size, and initial value.
createSlider :: (Double, Double) -> Double -> Double -> Now (HScale, Behavior Double)
createSlider (a,b) stepsize initval = do
    slider <- sync $ hScaleNewWithRange a b stepsize
    sync $ set slider [rangeValue := initval]
    changed <- getSignal valueChanged slider (rangeGetValue slider >>=)
    val <- sample $ fromChanges initval changed
    return (slider,val)

-- | Creates a slider which displays a dynamic value and emits attempts by the user to change that value.
createMotorizedSlider :: (Double, Double) -> Double -> Behavior Double -> Now (HScale,EvStream Double)
createMotorizedSlider (a,b) stepsize dat = do
    i <- sample dat
    slider <- sync $ hScaleNewWithRange a b stepsize
    setAttr rangeValue slider dat
    stream <- getSignal changeValue slider (\f _ d -> f d >> return True)
    return (slider,stream)
