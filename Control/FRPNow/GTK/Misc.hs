{-# LANGUAGE RecursiveDo #-}
module Control.FRPNow.GTK.Misc where

import Control.FRPNow.GTK.Core
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Control.Applicative
import Control.Monad
import Control.FRPNow
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import System.IO
import Control.Exception

setLockedFuturistic :: (WidgetClass w) => w -> Behavior Bool -> Now ()
setLockedFuturistic w lock = do
    done <- getUnrealize w
    let lockchange = toChanges lock `beforeEs` done
    callIOStream (widgetSetSensitive w . not) lockchange

runFileChooserDialog :: FileChooserDialog -> Now (Event (Maybe FilePath))
runFileChooserDialog dialog = do
    (retev, cb) <- callback
    sync $ mdo
        conn <- on dialog response $ \resp -> do
            widgetHide dialog
            case resp of
                ResponseAccept -> do
                    mfn <- fileChooserGetFilename dialog
                    cb mfn
                _ -> cb Nothing
            signalDisconnect conn
        widgetShow dialog
    return retev

nothingOnIOError :: IOError -> IO (Maybe a)
nothingOnIOError _ = return Nothing

widgetAddClasses :: WidgetClass widget => widget -> [Text] -> IO ()
widgetAddClasses w cs = do
    sc <- widgetGetStyleContext w
    forM_ cs $ \c -> styleContextAddClass sc c
