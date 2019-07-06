data GameState = GameState
  { tileBag :: [Tile]
  , players :: [Player]
  }

data Tile = Blank BlankTile | Letter LetterTile

data BlankTile = BlankTile

data LetterTile = LetterTile
  { letter :: Char
  , score :: Int
  }

data Player = Player
  { name :: String
  , tiles :: [Tile]
  }

data PlayerMessage = Pass | Play [AllocatedTile]

data Placement = PlayBlank BlankTile Char Position
  | PlayLetter Char Position

data AllocatedTile = AllocatedLetter LetterTile | AllocatedBlank BlankTile

data Position = Position
  { x :: Int
  , y :: Int
  }

main :: IO ()
main = putStrLn "Nothing implemented yet"
