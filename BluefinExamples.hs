module BluefinExamples where

import Bluefin.EarlyReturn
  ( EarlyReturn,
    returnEarly,
    withEarlyReturn,
  )
import Bluefin.Eff
  ( Eff,
    runEff,
    runPureEff,
    (:>),
  )
import Bluefin.IO (IOE, effIO)
import Bluefin.State (evalState, get, modify)
import Bluefin.Stream
  ( Stream,
    forEach,
    yield,
    yieldToList,
  )
import Control.Monad (when)
import Data.Foldable (for_)

totalPrice :: [Int] -> Int
totalPrice prices = runPureEff $ do
  evalState 0 $ \total -> do
    for_ prices $ \price -> do
      modify total (+ price)

    get total

-- ghci> totalPrice [1, 2, 3, 4, 5]
-- 15

containsItem :: String -> [String] -> Bool
containsItem soughtItem basket = runPureEff $ do
  withEarlyReturn $ \ret -> do
    for_ basket $ \basketItem -> do
      when (basketItem == soughtItem) $
        returnEarly ret True

    pure False

-- ghci> containsItem "Hat" ["Shirt", "Shorts", "Hat"]
-- True

-- ghci> containsItem "Hat" ["Shirt", "Shorts", "Belt"]
-- False

productCombinations ::
  ([String], String, [String]) -> [String]
productCombinations (colors, item, sizes) = runPureEff $ do
  (combinations, ()) <- yieldToList $ \y -> do
    for_ colors $ \color -> do
      for_ sizes $ \size -> do
        yield y (color <> " " <> item <> " " <> size)
  pure combinations

-- ghci> productCombinations (["Blue", "Green", "Yellow"], "shirt", ["S", "M", "L"])
-- ["Blue shirt S","Blue shirt M","Blue shirt L","Green shirt S","Green shirt M","Green shirt L","Yellow shirt S","Yellow shirt M","Yellow shirt L"]

productCombinationsEff ::
  (e1 :> es) =>
  ([String], String, [String]) ->
  Stream String e1 ->
  Eff es ()
productCombinationsEff (colors, item, sizes) y = do
  for_ colors $ \color -> do
    for_ sizes $ \size -> do
      yield y (color <> " " <> item <> " " <> size)

findMyItemEff ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  EarlyReturn Bool e2 ->
  ([String], String, [String]) ->
  String ->
  Eff es a
findMyItemEff io ret items soughtItem = do
  forEach (productCombinationsEff items) $ \item -> do
    when (item == soughtItem) $ do
      effIO io (putStrLn ("Found item: " <> soughtItem))
      returnEarly ret True

  effIO io (putStrLn ("Didn't find item: " <> soughtItem))
  returnEarly ret False

findMyItem :: ([String], String, [String]) -> String -> IO Bool
findMyItem items soughtItem = runEff $ \io -> do
  withEarlyReturn $ \ret ->
    findMyItemEff io ret items soughtItem

-- ghci> findMyItem (["Blue", "Green", "Yellow"], "shirt", ["S", "M", "L"]) "Yellow shirt M"
-- Found item: Yellow shirt M
-- True

-- ghci> findMyItem (["Blue", "Green", "Yellow"], "shirt", ["S", "M", "L"]) "Grey shirt XXL"
-- Didn't find item: Grey shirt XXL
-- False
