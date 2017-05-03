{-# LANGUAGE RecursiveDo #-}

{- |
Module      :  Control.FRPNow.GTK.Misc
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

Utility Functions which do not fit anywhere else in the package.

-}
module Control.FRPNow.GTK.Misc where

import Control.FRPNow.GTK.Core
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.FRPNow
import qualified Data.Text as T
import Data.Text (Text)
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

-- | Map an expensive computation over a 'Behavior' to WHNF in a background thread ('force' is useful to get deep evaluation). The output behavior will lag behind the input behavior and thus requires an initial value.
-- To control the running of the computation, this function also takes an event after which all changes are disregarded (can be provided by 'getUnrealize' to tie this ti widget lifetimes) and returns a boolean Behavior which indicated if a new result is currently pending.
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

-- | Filter an event so that it only resolves if it does so before a cutoff event.
beforeE :: Event a -> Event () -> Behavior (Event a)
beforeE ev cutoff = fmap join $ first (never <$ cutoff) (pure <$> ev)

-- | Disable a widget when the condition is true. Does not check the initial state iof the condition and assumes it to be False initially. This isso it can be used inside 'mfix' where the condition has not been defined yet..
setLockedFuturistic :: (WidgetClass w) => w -> Behavior Bool -> Now ()
setLockedFuturistic w lock = do
    done <- getUnrealize w
    let lockchange = toChanges lock `beforeEs` done
    callIOStream (widgetSetSensitive w . not) lockchange

-- | Run a FileChooserDialog without blocking and return an Evemnt containing the resulting selected path.
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

-- | Check any 'IOError's returned by the action so that they result in a 'Nothing' value.
checkIOError :: IO a -> IO (Maybe a)
checkIOError action = catch (fmap Just action) handler where
    handler :: IOError -> IO (Maybe a)
    handler _ = return Nothing

-- | Add CSS classes to a widget.
widgetAddClasses :: (MonadIO m, WidgetClass widget) => [Text] -> widget -> m widget
widgetAddClasses cs w = liftIO $ do
    sc <- widgetGetStyleContext w
    forM_ cs $ \c -> styleContextAddClass sc c
    return w
