module Control.FRPNow.GTK.Containers where

import Control.FRPNow.GTK.Core
import Graphics.UI.Gtk.General.Enums (Align(..))
import Graphics.UI.Gtk
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.FRPNow
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)


clearChildren :: (MonadIO m, ContainerClass w) => w -> m ()
clearChildren cont = liftIO $ do
    children <- containerGetChildren cont
    forM_ children widgetDestroy

set' :: (MonadIO m) => [AttrOp w] -> w -> m w
set' ops w = liftIO $ do
    set w ops
    return w

bpack :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bpack w = ReaderT $ \b -> boxPackStart b w PackNatural 0

bstretch :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bstretch w = ReaderT $ \b -> boxPackStart b w PackGrow 0

bspacer :: (BoxClass b) => ReaderT b IO ()
bspacer = ReaderT $ \b -> do
    s <- hBoxNew False 0
    boxPackStart b s PackGrow 10

gcell :: (WidgetClass w, GridClass g) => (Int,Int) -> w -> ReaderT g IO ()
gcell (x,y) w = ReaderT $ \g -> gridAttach g w x y 1 1

gcellspan :: (WidgetClass w, GridClass g) => (Int,Int) -> (Int,Int) -> w -> ReaderT g IO ()
gcellspan (x,y) (dx,dy) w = ReaderT $ \g -> gridAttach g w x y dx dy




createHBox :: (MonadIO m) => Int -> ReaderT HBox IO () -> m HBox
createHBox spacing filler = liftIO $ do
    b <- hBoxNew False spacing
    runReaderT filler b
    return b

createVBox :: (MonadIO m) => Int -> ReaderT VBox IO () -> m VBox
createVBox spacing filler = liftIO $ do
    b <- vBoxNew False spacing
    runReaderT filler b
    return b

createGrid :: (MonadIO m) => Int -> Int -> ReaderT Grid IO () -> m Grid
createGrid xspace yspace filler = liftIO $ do
    g <- gridNew
    gridSetRowSpacing g yspace
    gridSetColumnSpacing g xspace
    runReaderT filler g
    return g



createExpander :: (MonadIO m, WidgetClass w) => Text -> Bool -> w -> m Expander
createExpander lbl startpos w = liftIO $ do
    ex <- expanderNew lbl
    expanderSetExpanded ex startpos
    containerAdd ex w
    return ex


createHPaned :: (MonadIO m, WidgetClass w1, WidgetClass w2) => w1 -> w2 -> m HPaned
createHPaned l r = liftIO $ do
    p <- hPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p

createVPaned :: (MonadIO m, WidgetClass w1, WidgetClass w2) => w1 -> w2 -> m VPaned
createVPaned l r = liftIO $ do
    p <- vPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p



createFrame :: (MonadIO m, WidgetClass w) => ShadowType -> w -> m Frame
createFrame shad w = liftIO $ do
    f <- frameNew
    frameSetShadowType f shad
    containerAdd f w
    return f

createScrolledViewport :: (MonadIO m, WidgetClass w) => w -> m ScrolledWindow
createScrolledViewport w = liftIO $ do
    scr <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport scr w
    return scr

createScrolledWindow :: (MonadIO m, WidgetClass w) => w -> m ScrolledWindow
createScrolledWindow w = liftIO $ do
    scr <- scrolledWindowNew Nothing Nothing
    containerAdd scr w
    return scr
