import Actions
import Clock
import Dungeon
import Globals
import Macros
import Parser
import Syntax
import Verbs

-- Zork I, as opposed to II or III.
zorkNumber :: Int
zorkNumber = 1

-- A GameObject is a thing that can be picked up, dropped, or otherwise
-- manipulated by the player.
data GameObject = GameObject
  { size :: Int, -- Capacity taken up in the container
    capacity :: Int, -- Capacity of the container
    value :: Int, -- Value of the object when taken the first time
    tvalue :: Int -- Value of the object when placed in the trophy case
  }
  deriving (Show)

-- The default game object, which is used to initialize the game objects.
defaultGameObject :: GameObject
defaultGameObject =
  GameObject
    { size = 5,
      capacity = 0,
      value = 0,
      tvalue = 0
    }

main = putStrLn "Hello, Haskell!"
