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
import Control.Concurrent
import Control.Concurrent.MVar

mapBAsync :: (Eq a) => Event () -> b -> (a -> b) -> Behavior a -> Now (Behavior b, Behavior Bool)
mapBAsync stop yinit f xb = do
    xinit <- sample xb
    let xchanged = toChanges xb `beforeEs` stop
    (pendingSet, setPending) <- callbackStream
    (yset, sety) <- callbackStream
    pending <- sample $ fromChanges True (merge (False <$ yset) pendingSet)
    q <- sync $ newMVar xinit
    flip callStream xchanged $ \xs -> let
        x = last xs
        in sync $ tryTakeMVar q >> putMVar q x
    sync . forkIO . forever $ do
        x <- takeMVar q
        setPending True
        y <- evaluate (f x)
        sety y
        setPending False
    yb <- sample $ fromChanges yinit yset
    return (yb, pending)

beforeE :: Event a -> Event () -> Behavior (Event a)
beforeE ev cutoff = fmap join $ first (never <$ cutoff) (pure <$> ev)

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
