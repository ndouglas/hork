{-# LANGUAGE BinaryLiterals #-}

module Object where

import Data.Bits
import Data.Word (Word32)

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

newtype Attributes = Attributes Word32 deriving (Eq, Num, Bits, Show)

setAttr :: Attribute -> Attributes -> Attributes
setAttr attr attrs = attrs `setBit` fromEnum attr

clearAttr :: Attribute -> Attributes -> Attributes
clearAttr attr attrs = attrs `clearBit` fromEnum attr

checkAttr :: Attribute -> Attributes -> Bool
checkAttr attr attrs = attrs `testBit` fromEnum attr

defaultAttributes :: Attributes
defaultAttributes = Attributes 0

data Properties = Properties
  { -- z-string describing the object
    text :: String,
    -- weight or size of the object
    size :: Int,
    -- maximum weight or size that an object can hold
    capacity :: Int,
    -- value when taken/entered the first time
    value :: Int,
    -- value when placed in the trophy case
    tvalue :: Int
  }
  deriving (Show)

defaultProperties :: Properties
defaultProperties =
  Properties
    { text = "",
      size = 5,
      capacity = 0,
      value = 0,
      tvalue = 0
    }

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
