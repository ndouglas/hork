module Object where

import Attributes
import CommonTypes
import Data.Bits
import Data.Word (Word32)
import Properties

-- A GameObject is a thing that can be picked up, dropped, or otherwise
-- manipulated by the player.
data GameObject = GameObject
  { -- attributes of the object
    attrs :: Attributes,
    -- properties of the object
    props :: Properties
  }
  deriving (Show)

-- The default game object, which is used to initialize the game objects.
defaultGameObject :: GameObject
defaultGameObject =
  GameObject
    { attrs = defaultAttributes,
      props = defaultProperties
    }
