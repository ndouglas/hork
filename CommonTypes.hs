module CommonTypes where

newtype RoomId = RoomId String deriving (Eq, Show)

data Exit
  = To RoomId
  | Blocked String
  | Conditional (GameState -> Either String RoomId)

-- The GameState is a record that holds the state of the game.
data GameState = GameState
  { -- _Zork I_, as opposed to II or III.
    zorkNumber :: Int,
    -- the current room
    here :: RoomId
  }
  deriving (Show)

gameState :: GameState
gameState =
  GameState
    { zorkNumber = 1,
      here = RoomId "West of House"
    }
