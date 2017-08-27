{-# LANGUAGE RecursiveDo, OverloadedLists, ScopedTypeVariables, OverloadedStrings  #-}

{-
 - A heavily-commented, partly modified version of the stripped-down Reflex todo list from reflex-examples
 -}

module UI where

import Reflex
import Reflex.Dom

import Control.Lens
import Data.Functor
import Control.Arrow ((>>>))

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe

main :: IO ()
main = 
  mainWidget 
    (el "div" 
      (do
        el "h1"
          (text "Hash - the less-stupid shell")
        inputW
        pure ()))

data Command = Rm | Ls | Cd
  deriving (Show, Eq)

cmdDesc :: Text -> Maybe Command
cmdDesc "rm" = Just Rm
cmdDesc "ls" = Just Ls
cmdDesc "cd" = Just Cd
cmdDesc _    = Nothing

-- | Create an input text widget.
inputW 
  :: forall t m
   . MonadWidget t m 
  => m ()
inputW = do
  let 
    emptyColor = "style" =: "border-color: red"
    okColor    = "style" =: "border-color: green"

  rec 
    let
      sendE = input 
            & keypress Enter

      color =  input ^. textInput_value
           <&> \txt -> 
                  if (cmdDesc txt == Nothing) 
                  then emptyColor 
                  else okColor

    -- Create a textInput whose contents are reset to "" when the @send@
    -- signal fires.
    input <- textInput $ def 
                       & setValue .~ (sendE $> "")
                       & textInputConfig_attributes .~ color

    let descD = input ^. textInput_value <&> cmdDesc

    dynText (descD <&> fmap (show >>> T.pack) 
                   <&> fromMaybe "invalid command")

  pure ()

