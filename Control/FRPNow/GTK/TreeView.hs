{-# LANGUAGE RankNTypes #-}
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

tvColumn :: Text -> ReaderT (TreeViewColumn, store) IO b -> ReaderT (TreeView, store) IO ()
tvColumn title filler = ReaderT $ \(tv,model) -> do
    col <- treeViewColumnNew
    set col [treeViewColumnTitle := title]
    runReaderT filler (col,model)
    treeViewAppendColumn tv col
    return ()

tvTextDisplay :: (CellLayoutClass col, TreeModelClass (store a), TypedTreeModelClass store) => (a -> Text) -> ReaderT (col, store a) IO ()
tvTextDisplay f = ReaderT $ \(col,model) -> do
    cell <- cellRendererTextNew
    set cell [cellTextEditable := False]
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell model $ \row -> [cellText := f row]
    return ()

tvShowDisplay :: (CellLayoutClass col, TreeModelClass (store a), TypedTreeModelClass store, Show b) => (a -> b) -> ReaderT (col, store a) IO ()
tvShowDisplay f = ReaderT $ \(col,model) -> do
    cell <- cellRendererTextNew
    set cell [cellTextEditable := False]
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell model $ \row -> [cellText := show (f row)]
    return ()
