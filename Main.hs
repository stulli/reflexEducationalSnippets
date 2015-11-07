import Reflex (mapDyn, holdDyn, combineDyn, mconcatDyn, constDyn)
import Reflex.Dom (elDynAttr, elAttr, (=:), text, MonadWidget (..), Event (..),
        Dynamic(..), button, el, mainWidget, display, dynText, value, current,
        textArea, (&), mainWidgetWithCss, setValue, (.~), XhrRequestConfig (..),
        listWithKey, toggle, simpleList)
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, XhrResponse (..), XhrRequest (..))
import Reflex.Class (tag, constant, mergeList, appendEvents, leftmost)
import Reflex.Dynamic (updated, tagDyn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Default (def)

-- example 1
combiningTwoEvents :: MonadWidget t m => String -> m ()
combiningTwoEvents heading = do
    eMain <- el "div" $ do
        text heading
        ev1 <- el "div" $ do
            eBtn <- button "b1"
            return $ fmap (\v -> "i am ev1 !!!") eBtn  -- attach a string to the button event
        ev2 <- el "div" $ do
            eBtn <- button "b2"
            return $ fmap (\v -> "i am ev2 !!!") eBtn
        return $ leftmost [ev1, ev2]
    mainDyn <- holdDyn "init" eMain
    el "div" $ dynText $ mainDyn

-- example 2
combiningTwoEventsNested :: MonadWidget t m => String -> m ()
combiningTwoEventsNested heading = do
    text heading
    finalEvent <- el "div" $ do
        evs <- mapM row [1,2]
        return $ leftmost evs
    mainDyn' <- holdDyn "init" finalEvent
    el "div" $ dynText $ mainDyn'

row :: MonadWidget t m => Integer -> m (Event t String)
row number = do
    ev1 <- el "div" $ do
        eBtn <- button $ "b" ++ show number
        return $ fmap (\v -> "i am ev " ++ show number ++ "!!!") eBtn  -- attach a string to the button event
    return ev1

--example 3
nestedButtonsWithEvents :: MonadWidget t m => String -> m ()
nestedButtonsWithEvents heading = do
    el "div" $ text heading
    asyncEvent <- button "main"
    resultEvent <- getDataFromServerMock asyncEvent
    listDyn <- holdDyn [] resultEvent
    listElements <- el "div" $ do
        simpleList listDyn mkChild
    changeEvent <- mapDyn leftmost listElements
    let finalEvent = switch $ current changeEvent
    finalDynamic <- holdDyn "init" finalEvent
    el "div" $ dynText finalDynamic
    el "br" $ return ()

mkChild :: MonadWidget t m => Dynamic t String -> m (Event t String)
mkChild d = do
    ev <- el "div" $ do
        dynText d
        button "click me"
    return (fmap (\_ -> "button") ev)

-- main
main :: IO ()
main = do
    mainWidget $ do
        combiningTwoEvents "1" >> separator
        combiningTwoEventsNested "2" >> separator
        nestedButtonsWithEvents "3" >> separator

separator = do
    el "br" $ do return ()
    el "br" $ do return ()
    el "br" $ do return ()

getDataFromServerMock :: MonadWidget t m => Event t a -> m (Event t [String])
getDataFromServerMock event = do
    ev <- performAsyncRequestMock $ fmap (\x -> xhrRequest "GET" "myData" def) event
    return $ fmap (decodeServerData . _xhrResponse_body) ev

decodeServerData x = ["one", "two"] 

performAsyncRequestMock :: MonadWidget t m => Event t XhrRequest -> m (Event t XhrResponse)
performAsyncRequestMock ev = return $ fmap (\_ -> XhrResponse (Just $ T.pack "test")) ev

