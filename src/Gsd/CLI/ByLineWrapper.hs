module Gsd.CLI.ByLineWrapper where

import qualified System.Console.Byline as ByLine (askWithMenuRepeatedly)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Control.Monad.IO.Class


type ErrorDescription = String

askWithMenuRepeatedly :: (MonadIO m)
           => Menu availableActions            -- ^ The 'Menu' to display.
           -> Stylized          -- ^ The prompt.
           -> Stylized          -- ^ Error message.
           -> Byline m (Either ErrorDescription availableActions)
askWithMenuRepeatedly m prompt errprompt = do
  answer <- ByLine.askWithMenuRepeatedly m prompt errprompt
  case answer of
          Match action -> return $ Right action
          NoItems ->      return $ Left $ "unexpected NoItems returned"
          Other x ->      return $ Left $ "unexpected Other returned"