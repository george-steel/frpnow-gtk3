module Control.FRPNow.GTK.DataWidgets where

import Control.FRPNow.GTK.Core
import Graphics.UI.Gtk
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.FRPNow
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

createLabel :: (MonadIO m) => Text -> m Label
createLabel t = liftIO $ do
    l <- labelNew (Just t)
    set l [miscXalign := 0]
    return l

createLabelDisplay :: Behavior Text -> Now Label
createLabelDisplay s = do
     l <- sync $ labelNew (Nothing :: Maybe Text)
     sync $ set l [miscXalign := 0, labelWrap := True]
     setAttr labelLabel l s
     return l

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

createEntry :: String -> Now (Entry, Behavior String)
createEntry inittext = do
    entry <- sync $ entryNew
    sync $ set entry [entryText := inittext]
    edits <- getSignal editableChanged entry (entryGetText entry >>=)
    btext <- sample $ fromChanges inittext edits
    return (entry, btext)

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

createSpinEntry :: (Double, Double) -> Double -> Double -> Now (SpinButton, Behavior Double)
createSpinEntry (min,max) step initval = do
    sbtn <- sync $ spinButtonNewWithRange min max step
    sync $ spinButtonSetValue sbtn initval
    (changes, changecb) <- callbackStream
    sync $ onValueSpinned sbtn (changecb =<< spinButtonGetValue sbtn)
    val <- sample $ fromChanges initval changes
    return (sbtn,val)

createIntSpinEntry :: (Int, Int) -> Int -> Int -> Now (SpinButton, Behavior Int)
createIntSpinEntry (min,max) step initval = do
    sbtn <- sync $ spinButtonNewWithRange (fromIntegral min) (fromIntegral max) (fromIntegral step)
    sync $ spinButtonSetValue sbtn (fromIntegral initval)
    (changes, changecb) <- callbackStream
    sync $ onValueSpinned sbtn (changecb =<< spinButtonGetValueAsInt sbtn)
    val <- sample $ fromChanges initval changes
    return (sbtn,val)

createProgressBar :: Behavior (Maybe (Double, Text)) -> Now ProgressBar
createProgressBar mprogress = do
    let isactive = fmap isJust mprogress
        position = fmap (maybe 0 fst) mprogress
        lbl = fmap (maybe T.empty snd) mprogress
    bar <- sync progressBarNew
    setAttr progressBarFraction bar position
    setAttr progressBarText bar lbl
    setAttr widgetSensitive bar isactive
    return bar

createSimpleProgressBar :: Behavior Double -> Now ProgressBar
createSimpleProgressBar progress = do
    bar <- sync $ progressBarNew
    setAttr progressBarFraction bar progress
    return bar

createSpinner :: Behavior Bool -> Now Spinner
createSpinner b = do
    spn <- sync $ spinnerNew
    setAttr spinnerActive spn b
    return spn

createSlider :: (Double, Double) -> Double -> Double -> Now (HScale, Behavior Double)
createSlider (min,max) step initval = do
    slider <- sync $ hScaleNewWithRange min max step
    sync $ set slider [rangeValue := initval]
    changed <- getSignal valueChanged slider (rangeGetValue slider >>=)
    val <- sample $ fromChanges initval changed
    return (slider,val)


createMotorizedSlider :: (Double, Double) -> Double -> Behavior Double -> Now (HScale,EvStream Double)
createMotorizedSlider (min,max) step b = do
    i <- sample b
    slider <- sync $ hScaleNewWithRange min max step
    setAttr rangeValue slider b
    stream <- getSignal changeValue slider (\f _ d -> f d >> return True)
    return (slider,stream)
