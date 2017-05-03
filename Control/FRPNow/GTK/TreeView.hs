{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Control.FRPNow.GTK.TreeView
Copyright   :  (c) George Steel 2017
License     :  BSD3
Maintainer  :  george.steel@gmail.org

Functions for working with 'TreeView' and dynamic data.
-}

module Control.FRPNow.GTK.TreeView where

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

-- | Creates a TreeView displauying a dynamic list. The columns and cell renderers are defined using reader monads built from the other functions in this module.In this casem the cells must be polymorphic in the type of store they accept so as to ensure read-only behavior. Returns the TreeView and the curently-selected item.
createListDisplay :: (Eq a) => Behavior [a] -> (forall store . (TreeModelClass (store a), TypedTreeModelClass store) => ReaderT (TreeView, store a) IO b) -> Now (TreeView, Behavior (Maybe a))
createListDisplay datafeed filler = do
    tv <- sync $ treeViewNew
    initdata <- sample datafeed
    store <- sync $ listStoreNew initdata
    sync $ treeViewSetModel tv store
    sync $ runReaderT filler (tv,store)
    flip callStream (toChanges datafeed) $ \newdata' -> sync $ do
        let newdata = last newdata'
        listStoreClear store
        mapM_ (listStoreAppend store) newdata
    itemSelected <- getSignal cursorChanged tv $ \cb -> do
        mcur <- fst <$> treeViewGetCursor tv
        case mcur of
            [i] -> do
                x <- listStoreGetValue store i
                cb (Just x)
            _ -> cb Nothing
    selItem <- sample $ fromChanges Nothing itemSelected
    return (tv,selItem)

-- | Add a column with the given title and renderers to the enclosing TreeView.
tvColumn :: Text -> ReaderT (TreeViewColumn, store) IO b -> ReaderT (TreeView, store) IO TreeViewColumn
tvColumn title filler = ReaderT $ \(tv,model) -> do
    col <- treeViewColumnNew
    set col [treeViewColumnTitle := title]
    runReaderT filler (col,model)
    treeViewAppendColumn tv col
    return col

-- | Add a cell renderer diaplaying a text-valued function of a row to a column.
tvTextDisplay :: (CellLayoutClass col, TreeModelClass (store a), TypedTreeModelClass store) => (a -> Text) -> ReaderT (col, store a) IO CellRendererText
tvTextDisplay f = ReaderT $ \(col,model) -> do
    cell <- cellRendererTextNew
    set cell [cellTextEditable := False]
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell model $ \row -> [cellText := f row]
    return cell

-- | Add a cell renderer diaplaying a s function of a row using Show. Useful for Int values.
tvShowDisplay :: (CellLayoutClass col, TreeModelClass (store a), TypedTreeModelClass store, Show b) => (a -> b) -> ReaderT (col, store a) IO CellRendererText
tvShowDisplay f = ReaderT $ \(col,model) -> do
    cell <- cellRendererTextNew
    set cell [cellTextEditable := False]
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell model $ \row -> [cellText := show (f row)]
    return cell
