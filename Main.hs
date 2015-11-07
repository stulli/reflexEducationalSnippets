import Reflex (mapDyn, holdDyn, combineDyn, mconcatDyn, constDyn)
import Reflex.Dom (elDynAttr, elAttr, (=:), text, MonadWidget (..), Event (..),
        Dynamic(..), button, el, mainWidget, display, dynText, value, current,
        textArea, (&), mainWidgetWithCss, setValue, (.~), XhrRequestConfig (..),
        listWithKey, toggle, simpleList)
import Reflex.Dom.Xhr (performRequestAsync, xhrRequest, XhrResponse (..), XhrRequest (..))
import Reflex.Class (tag, constant, mergeList, appendEvents, leftmost)
import Reflex.Dynamic (updated, tagDyn)
import qualified Data.List.NonEmpty as NE
import Data.Char (toUpper)
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

-- example 3
-- What this snippet does:
-- 1. Get data (a list of strings) via Ajax request (mocked)
-- 2. Dynamically create a list where each item
--    - is labelled with data from the data request 
--    - has a button that returns data based on the label
-- 3. Clicks on the buttons of the list items will change the result div
nestedButtonsWithEvents :: MonadWidget t m => String -> m ()
nestedButtonsWithEvents heading = do
    el "div" $ text heading
    -- click on the main button fetches some data
    asyncEvent <- button "main"
    resultEvent <- getDataFromServerMock asyncEvent
    -- create list
    listDyn <- holdDyn [] resultEvent
    listElements <- el "div" $ do
        simpleList listDyn mkChild
    -- simply take the first event from the list
    iAmStillADynamic <- mapDyn leftmost listElements
    -- here we extract the event from the dynamic
    let finalEvent = switch $ current iAmStillADynamic
    -- after extracting another dynamic is created
    -- (seems like this can somehow be optimized)
    finalDynamic <- holdDyn "init" finalEvent
    el "div" $ dynText finalDynamic

mkChild :: MonadWidget t m => Dynamic t String -> m (Event t String)
mkChild d = do
    ev <- el "div" $ do
        dynText d
        button "click me"
    modifiedDynamic <- mapDyn (map toUpper) d
    return $ tagDyn modifiedDynamic ev

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

