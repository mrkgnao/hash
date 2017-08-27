{-# LANGUAGE RecursiveDo, OverloadedLists, ScopedTypeVariables, OverloadedStrings  #-}

{-
 - A heavily-commented, partly modified version of the stripped-down Reflex todo list from reflex-examples
 -}

module MiniTodo where

import Reflex
import Reflex.Dom

import Control.Lens
import Data.Functor
import Control.Arrow ((>>>))

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

type IMap a = M.Map Int a

-- Add a new value to a map, automatically choosing an unused key.
new :: a -> IMap a -> IMap a
new v m = case M.maxViewWithKey m of
    Nothing -> [(0, v)] -- overloadedlists
    Just ((k, _), _) -> M.insert (succ k) v m

-- | Create an input text widget with auto reset on return and return an event 
-- firing on return containing the string before reset.
inputW 
  :: forall t m
   . MonadWidget t m 
  => m (Event t Text)
inputW = do
    rec 
      let 
        -- Fire a signal on a *return* key press.
        sendE = input ^. textInput_keypress
              & ffilter (== 13)

        -- Dynamic CSS attributes, inspired by the calculator in
        -- the reflex-platform tutorial.
        emptyColor = "style" =: "border-color: red"
        okColor    = "style" =: "border-color: green"

        color      = input ^. textInput_value
                 <&> (\txt -> if txt == "" then emptyColor else okColor)

      -- Create a textInput whose contents are reset when the @send@
      -- signal fires.
      input <- textInput $ def 
                         & setValue .~ (sendE $> "")
                         & textInputConfig_attributes .~ color

    -- Tag the send signal with the contents of the input field
    -- *before* resetting the contents.
    let todoText = sendE
                 & tag (current (input ^. textInput_value))

    pure todoText

type UserAction = IMap Text -> IMap Text

-- | Given an input signal, create text labels for todo items with contents 
-- given by the input. Listen for deletion events on those text labels, and
-- create a reactive todo list that always reflects the current state based
-- on these two signals (todo creation and deletion).
listW :: forall t m
       . MonadWidget t m 
      => Event t Text -> m ()
listW evt = do
    let initialState = M.empty

    rec
        -- Starting with the initial map of todo items, arrive at the current 
        -- state by applying all the insertions and deletions made by the user.
        --
        -- Instead of applying them one after the other, we compose them all and
        -- then create a @Dynamic@ map by executing this combined modification
        -- upon the initial state.
        items <- [ insertActions
                 , deleteActions
                 ] & mergeWith (.)
                   & foldDyn ($) initialState

        -- Generate an unordered list of items from the map of todo item contents.
        -- @itemsUlOf@ gives us a @Dynamic@ map of deletion @Event@s which 
        -- trigger on the deletion of the corresponding todo items.
        deleteSignals <- itemsUlOf items

        let 
          insertActions, deleteActions :: Event t UserAction

          -- When @evt@ fires, convert it to a function that, given a map, 
          -- inserts the text of @evt@ (which is the todo item) into the map.
          insertActions = evt <&> new

          -- Convert the deletion signals into a function that applies all the 
          -- deletion actions to our state.
          deleteActions = current deleter        -- Behavior t (Event t UserAction)
                        & switch
          
          -- Create a Dynamic t (Event t (IMap a -> IMap a)), in stages
          -- involving types of the form Dynamic t (f (Event t a)). I will 
          -- write this as Dynamic (f (Event a)).
          --
          -- That is, I'm suppressing the @t@ type parameter for clarity.

          deleter     :: Dynamic t (Event t UserAction)
          deleter     =  deleteSignals            -- Dynamic (IMap (Event Int))
                                                  -- our map of deletion signals
                                          
                     <&> M.elems                  -- Dynamic [Event Int]
                                                  -- list of values
                                                 
                     <&> map (<&> M.delete)       -- Dynamic [Event (IMap a -> IMap a)]
                                                  -- turn each v into a function that deletes
                                                  -- elements with key v from a map
                                                 
                     <&> mergeWith (.)            -- Dynamic (Event (IMap a -> IMap a))
                                                  -- combine all those endomorphisms
    pure ()

-- Output the ul of the elements of the given map and return the delete event 
-- for each key.
itemsUlOf 
  :: forall t m
   . MonadWidget t m 
  =>    Dynamic t (IMap Text) 
  -> m (Dynamic t (IMap (Event t Int)))
itemsUlOf xs = 
  elClass "ul" "list"
    (listWithKey xs todoItem)

todoItem
  :: forall t m
   . MonadWidget t m 
  => Int -> Dynamic t Text -> m (Event t Int)
todoItem key txt =
  elClass "li" "element" $ do
    -- Create a dynamic text label.
    dynText txt
    -- When the delete button is pressed, tag the onClick signal
    -- with the key @k@ of this label.
    elClass "div" "delete" (button "delete") <&> ($> key)

main :: IO ()
main = 
  mainWidget 
    (el "div" 
      (inputW >>= listW))
