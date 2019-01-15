module Gsd.CLI.ByLineWrapper where

import qualified System.Console.Byline as ByLine (askWithMenuRepeatedly)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Control.Monad.IO.Class


type ErrorDescription = String

askWithMenuRepeatedly :: (MonadIO m)
           => Menu availableActions            -- ^ The 'Menu' to display.
           -> Stylized          -- ^ The prompt.
           -> Stylized          -- ^ Error message.
           -> Byline m availableActions
askWithMenuRepeatedly m prompt errprompt = do
  answer <- ByLine.askWithMenuRepeatedly m prompt errprompt
  case answer of
          Match action -> return action
          NoItems ->      error "unexpected NoItems returned"
          Other x ->      error "unexpected Other returned"