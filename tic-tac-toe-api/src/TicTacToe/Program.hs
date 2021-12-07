module TicTacToe.Program
  ( play
  , Play(..)
  , Player(..)
  , type Position
  ) where

import           Relude


data Err = AlreadyChosenPosition
         -- | OutOfBoundPosition
         | AlreadyFinishedGame
         -- | ChoiceDoesNotBelongToThisUser
         | UnknownError
    deriving Show

data Choice = Circle | Cross
  deriving Show

data Player (a :: Choice) = Player Text
  deriving Show

type PlayerA = Player 'Circle
type PlayerB = Player 'Cross

type FreePositions = [Position]

data Status = Winner (Either PlayerA PlayerB)
            | NoOneWins
            | Start
            | ContinuePlaying FreePositions
            deriving Show

data Board (r :: Status) = Board [[Maybe (Either PlayerA PlayerB)]]

type Position = (Int, Int)

data Play = Play (Either PlayerA PlayerB, Position)

play :: forall i (s :: Status) 
     . Board ('ContinuePlaying i)
     -> Play
     -> Either Err (Board s)
play _ _ = Left UnknownError

program :: Either Err (Board 'NoOneWins)
program = 
  let board = Board [[Nothing,Nothing]] :: Board ('ContinuePlaying i)
      firstMove = play board (Play (Left (Player "Juan"), (1,1)))
   in case firstMove of
        Right something -> play something (Play (Right (Player "Doe"), (2,1))) 
        _ -> Right (Board [[]] :: Board 'NoOneWins)

