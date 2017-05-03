{- |
Module      :  Control.FRPNow.GTK.Containers
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

EDSL for widget layouts ussing GTK container widgets.

Containers containing single children are created by actions taking the child widget as their last parameter, allowing for easy composition using '=<<'.
Containers taking multiple children take their contents as a reader monad which runs over the newly-created widget, allowing packing commands to insert children of heterogeneous types, possibly with options.
As all container-creation actions run in 'MonadIO', nesting containers can be done inline with standard monadic composition.

Given the wiegets foo, bar, baz, where foo supports scrolling (such as a TreeView) we can create a simple layout as follows

> layout <- createVBox 0 $ do
>     bstretch =<< createFrame ShadowIn =<< createScrolledWindow foo
>     bpack <=< createHBox 0 $ do
>         bpack bar
>         bspacer
>         bpack =<< set' [attr := val] baz

-}

module Control.FRPNow.GTK.Containers where

import Graphics.UI.Gtk
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Text (Text)

-- | Destroys all children in a container, leaving it empty
clearChildren :: (MonadIO m, ContainerClass w) => w -> m ()
clearChildren cont = liftIO $ do
    children <- containerGetChildren cont
    forM_ children widgetDestroy

-- | Sets aattributes on a widget and returns the widget. Useful for setting attributes inline in a composition chain.
set' :: (MonadIO m) => [AttrOp w] -> w -> m w
set' ops w = liftIO $ do
    set w ops
    return w

-- * Box

-- | Creates an HBox with a given spacing and fills it using the reader.
createHBox :: (MonadIO m) => Int -> ReaderT HBox IO a -> m HBox
createHBox spacing filler = liftIO $ do
    b <- hBoxNew False spacing
    runReaderT filler b
    return b

-- | Creates a VBox with a given spacing and fills it using the reader.
createVBox :: (MonadIO m) => Int -> ReaderT VBox IO a -> m VBox
createVBox spacing filler = liftIO $ do
    b <- vBoxNew False spacing
    runReaderT filler b
    return b

-- | Inserts a witget into the enclosing box as its natural size.
bpack :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bpack w = ReaderT $ \b -> boxPackStart b w PackNatural 0

-- | Inserts a widget into the enclosing box with rubber length (takes all excess space).
bstretch :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bstretch w = ReaderT $ \b -> boxPackStart b w PackGrow 0

-- | Inserts an expanding spacer into a box
bspacer :: (BoxClass b) => ReaderT b IO ()
bspacer = ReaderT $ \b -> do
    s <- hBoxNew False 0
    boxPackStart b s PackGrow 10


-- * Grid

-- | Creates a Grid (with the given x and y spacing) and fills it using the reader.
createGrid :: (MonadIO m) => Int -> Int -> ReaderT Grid IO () -> m Grid
createGrid xspace yspace filler = liftIO $ do
    g <- gridNew
    gridSetRowSpacing g yspace
    gridSetColumnSpacing g xspace
    runReaderT filler g
    return g

-- | Inserts a widget into the enclosing Grid at the given (x,y) position.
gcell :: (WidgetClass w, GridClass g) => (Int,Int) -> w -> ReaderT g IO ()
gcell (x,y) w = ReaderT $ \g -> gridAttach g w x y 1 1

-- | Inserts a widget into the enclosign Grid spanning several cells, with position fiven by the first srgument and size given by the second.
gcellspan :: (WidgetClass w, GridClass g) => (Int,Int) -> (Int,Int) -> w -> ReaderT g IO ()
gcellspan (x,y) (dx,dy) w = ReaderT $ \g -> gridAttach g w x y dx dy


-- * Stack

-- | Creates a Stack and fills it using the reader.
createStack :: (MonadIO m) => ReaderT Stack IO () -> m Stack
createStack filler = liftIO $ do
    s <- stackNew
    runReaderT filler s
    return s

-- | Adds an element to the enclosing Stack with the given key.
stackElem :: (WidgetClass w) => Text -> w -> ReaderT Stack IO ()
stackElem key w = ReaderT $ \s -> stackAddNamed s w key

-- | Adds an element to the enclosing Stack with the given key and title.
stackElemTitled :: (WidgetClass w) => Text -> Text -> w -> ReaderT Stack IO ()
stackElemTitled key title w = ReaderT $ \s -> stackAddTitled s w key title

-- | Creates a StackSwitcher for a given Stack
createStackSwitcher :: (MonadIO m) => Stack -> m StackSwitcher
createStackSwitcher s = liftIO $ do
    sw <- stackSwitcherNew
    stackSwitcherSetStack sw s
    return sw


-- * Notebook

-- | Createa a Notebook and fills it using the reader.
createNotebook :: (MonadIO m) => ReaderT Notebook IO a -> m Notebook
createNotebook filler = liftIO $ do
    s <- notebookNew
    runReaderT filler s
    return s

-- | Add a tab to the enclosing Notebook with the given title
nbpage :: (WidgetClass w) => Text -> w -> ReaderT Notebook IO ()
nbpage lbl w = ReaderT $ \nb -> void (notebookAppendPage nb w lbl)


-- * Scrolling

-- | Creates a ScrolledWindow around a widget supporting scrolling natively.
createScrolledWindow :: (MonadIO m, WidgetClass w) => w -> m ScrolledWindow
createScrolledWindow w = liftIO $ do
    scr <- scrolledWindowNew Nothing Nothing
    containerAdd scr w
    return scr

-- | Creates a ScrolledWindow and Vieqwport around a widget not supporting scrollign such as a Box or Grid.
createScrolledViewport :: (MonadIO m, WidgetClass w) => w -> m ScrolledWindow
createScrolledViewport w = liftIO $ do
    scr <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport scr w
    return scr


-- * Misc

-- | Creates a frame around a widget with a given shadow type
createFrame :: (MonadIO m, WidgetClass w) => ShadowType -> w -> m Frame
createFrame shad w = liftIO $ do
    f <- frameNew
    frameSetShadowType f shad
    containerAdd f w
    return f

-- | Creates an Expander around a given widget. The Text and Bool parameters control the label of the expander and whether it starts expanded.
createExpander :: (MonadIO m, WidgetClass w) => Text -> Bool -> w -> m Expander
createExpander lbl startpos w = liftIO $ do
    ex <- expanderNew lbl
    expanderSetExpanded ex startpos
    containerAdd ex w
    return ex

-- | Creates an HPaned contsing two other widgets.
createHPaned :: (MonadIO m, WidgetClass w1, WidgetClass w2) => w1 -> w2 -> m HPaned
createHPaned l r = liftIO $ do
    p <- hPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p
-- | Creates a VPaned containing two other widgets.
createVPaned :: (MonadIO m, WidgetClass w1, WidgetClass w2) => w1 -> w2 -> m VPaned
createVPaned l r = liftIO $ do
    p <- vPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p
