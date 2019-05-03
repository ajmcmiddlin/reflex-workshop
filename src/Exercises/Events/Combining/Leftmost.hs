{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.Events.Combining.Leftmost (
    leftmostExercise
  ) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex

leftmostExercise :: Reflex t
                 => Event t Int
                 -> ( Event t Text
                    , Event t Text
                    , Event t Text
                    , Event t Text
                    )
leftmostExercise eIn =
  let
    f = mof 3 "Fizz" eIn
    b = mof 5 "Buzz" eIn
    fb = f <> b
    t = Text.pack . show <$> eIn
    -- originally you had `f` and `b` in this list. Note that `fb` takes care of
    -- this for us as it combines the outputs of the events, so we get "",
    -- "Fizz", or "FizzBuzz" depending on which events fire.
    sol = leftmost [fb, t]
  in
    (f, b, fb, sol)

mof ::
  Reflex t
  => Int
  -> Text
  -> Event t Int
  -> Event t Text
mof a t =
  (t <$) . ffilter ((== 0) . (`mod` a))
