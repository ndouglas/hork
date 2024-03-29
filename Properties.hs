module Properties where

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
    -- synonyms for the object
    synonyms :: [String]
  }
  deriving (Show)

defaultProperties :: Properties
defaultProperties =
  Properties
    { text = "",
      size = 5,
      capacity = 0,
      value = 0,
      tvalue = 0,
      fdesc = "",
      ldesc = "",
      health = 0,
      adjectives = [],
      synonyms = []
    }
