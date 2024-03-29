{-# LANGUAGE BinaryLiterals #-}

module CommonTypes where

import Data.Bits
import Data.Map qualified as Map
import Data.Maybe qualified
import Data.Word (Word32)

-- The Attribute type represents the possible attributes of an object.
data Attribute
  = -- Room is part of the maze.
    MazeBit
  | -- Room is part of the house.
    HouseBit
  | -- Room is on dry land.
    RLandBit
  | -- For objects, it gives light. For locations, it is lit.
    -- All outdoor rooms should have ONBIT set.
    OnBit
  | -- Object can be a source of fire.
    -- LIGHTBIT should also be set.
    FlameBit
  | -- Object can be entered or boarded by the player.
    VehicleBit
  | -- Object can be turned on or off.
    LightBit
  | -- Object can cut other objects.
    KnifeBit
  | -- Object can be burned.
    BurnBit
  | -- Object can be read.
    ReadBit
  | -- Object is a container and hold objects which are always visible.
    -- CONTBIT and OPENBIT should be set as well.
    SurfaceBit
  | -- Object can be turned on or off.
    SwitchBit
  | -- Object could be picked up but other values/ routines need to be checked.
    TryTakeBit
  | -- Object can be opened or closed; refers to doors and containers.
    OpenBit
  | -- Object is a container and can contain other objects or be open/closed/
    -- transparent.
    ContBit
  | -- Object is transparent so objects inside it can be found even if OPENBIT
    -- is clear.
    TransBit
  | -- Object can be eaten.
    FoodBit
  | -- Object can be picked up or carried.
    TakeBit
  | -- Object can accept other objects.
    AcceptBit
  | -- Thief cannot enter this room.
    SacredBit
  | -- Object is a character in the game.
    PersonBit
  | -- Object is a door.
    DoorBit
  | -- Object can be drunk.
    DrinkBit
  | -- Object can be used as a tool to open other things.
    ToolBit
  | -- Object can be climbed.
    ClimbBit
  | -- Object cannot be taken separately from other objects; it is part of
    -- another object.
    IntegralBit
  | -- Object is injured but not dead.
    InjuredBit
  | -- Object is alive.
    AliveBit
  | -- For object, it has been taken or used. For rooms, it has been visited.
    TouchedBit
  | -- Object is not detected by the game.
    InvisibleBit
  | -- Object can't be entered (or full of water bit).
    CantEnterBit
  | -- Room is in or near the water.
    NonLandBit
  deriving (Enum, Show)

-- The Attributes type represents the attributes of an object.
newtype Attributes = Attributes Word32 deriving (Eq, Num, Bits, Show)

-- Set an attribute bit.
setAttr :: Attribute -> Attributes -> Attributes
setAttr attr attrs = attrs `setBit` fromEnum attr

-- Clear an attribute bit.
clearAttr :: Attribute -> Attributes -> Attributes
clearAttr attr attrs = attrs `clearBit` fromEnum attr

-- Check whether an attribute bit is set.
checkAttr :: Attribute -> Attributes -> Bool
checkAttr attr attrs = attrs `testBit` fromEnum attr

-- The default attributes for an object.
defaultAttributes :: Attributes
defaultAttributes = Attributes 0

-- The Properties type represents the properties of an object.
data Properties = Properties
  { -- description of the object
    desc :: String,
    -- z-string describing the object
    text :: String,
    -- weight or size of the object
    size :: Int,
    -- maximum weight or size that an object can hold
    capacity :: Int,
    -- value when taken/entered the first time
    value :: Int,
    -- value when placed in the trophy case
    tvalue :: Int,
    -- z-string with first description of the object
    fdesc :: String,
    -- z-string with "lying" description of the object
    -- this is used only for an object that has not been disturbed
    ldesc :: String,
    -- health (0-5; 0 is healthy, 5 is dead)
    health :: Int,
    -- adjectives describing the object
    adjectives :: [String],
    -- synonyms for the object, including primary name
    synonyms :: [String],
    -- exits from the room
    exits :: Map.Map Direction Exit,
    -- starting location
    location :: ObjectId,
    -- action associated with this object
    action :: Maybe RoomAction
  }
  deriving (Show)

-- Determine whether an object is open.
isObjectOpen :: GameState -> ObjectId -> Bool
isObjectOpen gameState objectId =
  case Map.lookup objectId (objects gameState) of
    Just entity -> checkAttr OpenBit (attrs entity)
    Nothing -> False -- Object not found (WTF?)

-- The default properties for an object.
defaultProperties :: Properties
defaultProperties =
  Properties
    { desc = "",
      text = "",
      size = 5,
      capacity = 0,
      value = 0,
      tvalue = 0,
      fdesc = "",
      ldesc = "",
      health = 0,
      adjectives = [],
      synonyms = [],
      exits = mempty,
      location = ObjectId "Rooms",
      action = Nothing
    }

-- A room-argument is passed to the action routine.
data RoomArgument
  = -- When a room is entered.
    Enter
  | -- When a room is exited.
    Exit
  | -- When a room is looked at.
    Look
  | -- At the beginning of a turn.
    Begin
  | -- At the end of a turn.
    End
  deriving (Show)

-- The RoomAction is a function associated with a room.
type RoomAction = RoomArgument -> GameState -> GameState

instance Show RoomAction where
  show _ = "RoomAction"

-- The ObjectId type represents the unique identifier for an object.
newtype ObjectId = ObjectId String deriving (Eq, Ord, Show)

-- A condition that can be evaluated to determine whether the player can exit a
-- room.
data ExitCondition
  = -- The player can always exit the room.
    AlwaysTrue
  | -- The player can exit the room if the specified global flag is set.
    -- e.g. GlobalFlag "WON-FLAG"
    GlobalFlag String
  | -- The player can exit the room if the specified object has the specified
    -- attribute bit set. e.g. ObjectState "Wooden Door" OpenBit
    ObjectAttribute ObjectId Attribute
  | -- For providing a specific message when the player cannot exit the room.
    Else ExitCondition String
  deriving (Show)

-- Evaluate an exit condition to determine if the player can exit the room.
evaluateExit :: GameState -> Exit -> Either String ObjectId
-- The player can always exit the room.
evaluateExit gameState (Unconditional objectId) = Right objectId
-- The player cannot move in this direction; a message is displayed.
evaluateExit gameState (NoExit msg) = Left msg
-- A condition determines whether the player can move in this direction.
evaluateExit gameState (Conditional cond objectId msg) =
  case cond of
    -- The player can always exit the room.
    AlwaysTrue -> Right objectId
    -- The player can exit the room if the specified global flag is set.
    GlobalFlag flagName ->
      if checkGlobalFlag gameState flagName
        then Right objectId
        else Left msg
    -- The player can exit the room if the specified object has the specified
    -- attribute bit set.
    ObjectAttribute objectId stateCheck ->
      case Map.lookup objectId (objects gameState) of
        Nothing -> Left "Object not found"
        Just entity ->
          if checkAttr stateCheck (attrs entity)
            then Right objectId
            else Left msg
    -- For providing a specific message when the player cannot exit the room.
    Else condition msg -> evaluateExit gameState (NoExit msg)

-- The Exit type represents the possible exits from a room.
data Exit
  = -- The player can move to the specified room.
    Unconditional ObjectId
  | -- The player cannot move in this direction; a message is displayed.
    NoExit String
  | -- A condition determines whether the player can move in this direction.
    -- Otherwise, a message is displayed.
    Conditional ExitCondition ObjectId String

-- The Show instance for Exit is used to display the exit when the player types "exits".
instance Show Exit where
  show (Unconditional objectId) = "To " ++ show objectId
  show (NoExit reason) = "Blocked " ++ show reason
  show (Conditional condition objectId reason) = "Conditional condition objectId reason"

-- The Direction type represents the possible directions the player can move.
data Direction = North | South | East | West | NE | NW | SE | SW | Up | Down | Out | In | Land
  deriving (Eq, Ord, Show)

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

-- The "Rooms" object contains all the rooms in the game.
rooms :: GameObject
rooms =
  GameObject
    { attrs = defaultAttributes,
      props =
        defaultProperties
          { exits = Map.fromList [(In, Unconditional (ObjectId "Rooms"))]
          }
    }

type ObjectLookup = Map.Map ObjectId GameObject

-- The GameState is a record that holds the state of the game.
data GameState = GameState
  { -- _Zork I_, as opposed to II or III.
    zorkNumber :: Int,
    -- the current room
    here :: ObjectId,
    -- object lookup
    objects :: ObjectLookup,
    -- global flags
    globalFlags :: Map.Map String Bool
  }
  deriving (Show)

-- The initial game state.
gameState :: GameState
gameState =
  GameState
    { zorkNumber = 1,
      here = ObjectId "West of House",
      objects = Map.fromList [(ObjectId "Rooms", rooms)],
      globalFlags = Map.fromList [("WON-FLAG", False)]
    }

-- Set a global flag.
setGlobalFlag :: GameState -> String -> Bool -> GameState
setGlobalFlag gameState flagName flagValue =
  gameState {globalFlags = Map.insert flagName flagValue (globalFlags gameState)}

-- Check whether a global flag is set.
checkGlobalFlag :: GameState -> String -> Bool
checkGlobalFlag gameState flagName =
  Data.Maybe.fromMaybe
    False
    (Map.lookup flagName (globalFlags gameState))

-- Insert an object into the game state.
addObject :: GameState -> ObjectId -> GameObject -> GameState
addObject gameState objectId object =
  gameState {objects = Map.insert objectId object (objects gameState)}

-- Set every attribute in a provided list to true and return the resulting
-- attributes.
withSetAttrs :: [Attribute] -> Attributes -> Attributes
withSetAttrs rest attrs = foldl (flip setAttr) attrs rest

-- <ROOM WEST-OF-HOUSE
--       (IN ROOMS)
--       (DESC "West of House")
--       (NORTH TO NORTH-OF-HOUSE)
--       (SOUTH TO SOUTH-OF-HOUSE)
--       (NE TO NORTH-OF-HOUSE)
--       (SE TO SOUTH-OF-HOUSE)
--       (WEST TO FOREST-1)
--       (EAST "The door is boarded and you can't remove the boards.")
--       (SW TO STONE-BARROW IF WON-FLAG)
--       (IN TO STONE-BARROW IF WON-FLAG)
--       (ACTION WEST-HOUSE)
--       (FLAGS RLANDBIT ONBIT SACREDBIT)
--       (GLOBAL WHITE-HOUSE BOARD FOREST)>

-- <ROUTINE WEST-HOUSE (RARG)
-- 	 <COND (<EQUAL? .RARG ,M-LOOK>
-- 		<TELL
-- "You are standing in an open field west of a white house, with a boarded
-- front door.">
-- 		<COND (,WON-FLAG
-- 		       <TELL
-- " A secret path leads southwest into the forest.">)>
-- 		<CRLF>)>>

-- The "West of House" room.
westOfHouse :: GameObject
westOfHouse =
  GameObject
    { attrs = withSetAttrs [RLandBit, OnBit, SacredBit] defaultAttributes,
      props =
        defaultProperties
          { desc = "West of House",
            exits =
              Map.fromList
                [ (North, Unconditional (ObjectId "North of House")),
                  (South, Unconditional (ObjectId "South of House")),
                  (NE, Unconditional (ObjectId "North of House")),
                  (SE, Unconditional (ObjectId "South of House")),
                  (West, Unconditional (ObjectId "Forest 1")),
                  (East, NoExit "The door is boarded and you can't remove the boards."),
                  (SW, Conditional (GlobalFlag "WON-FLAG") (ObjectId "Stone Barrow") "You can't go that way."),
                  (In, Conditional (GlobalFlag "WON-FLAG") (ObjectId "Stone Barrow") "You can't go that way.")
                ],
            adjectives = ["white", "house", "board", "forest"],
            location = ObjectId "Rooms"
          }
    }