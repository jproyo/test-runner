module Data.Mondrian where

import           Relude
import           Test.QuickCheck

data Point = Point
  { _pX :: Int
  , _pY :: Int
  , _pColor :: RGB
  }

data Line = Line
  { _lOrigin :: Point
  , _lDest   :: Point
  }

isVertical :: Line -> Bool
isVertical Line {..} = _pX _lOrigin == _pX _lDest

isHorizontal :: Line -> Bool
isHorizontal Line {..} = _pY _lOrigin == _pY _lDest

newtype DimensionFrame = DimensionFrame (Int, Int)

newtype RGB = RGB ByteString

data FillSquare = FillSquare
  { _fsSquare :: [Point] -- Refine type with Nat
  , _fsColor  :: RGB
  }

newtype Lines = Lines [Line]
  deriving newtype (Monoid, Semigroup)

newtype FilledSquares = FilledSquares [FillSquare]

data MondrianPainting = MondrianPainting
  { _mpLines         :: Lines
  , _mpFilledSquares :: FilledSquares
  }

generatePoint :: DimensionFrame -> IO Point
generatePoint (DimensionFrame (x, y)) =
  Point <$> choose (0, x) <*> choose (0, y)

generateLine :: DimensionFrame -> IO Line
generateLine dim = Line <$> generatePoint dim <*> generatePoint dim

generateLines :: (Line -> Bool) -> DimensionFrame -> Int -> IO Lines
generateLines pred dim number =
  replicate number $ generate $ suchThat pred $ generateLine dim

generateVerticalLines :: DimensionFrame -> Int -> IO Lines
generateVerticalLines = generateLines isVertical

generateHorizontalLines :: DimensionFrame -> Int -> IO Lines
generateHorizontalLines = generateLines isHorizontal

generateFourSquarePoints ::  DimensionFrame -> Lines -> IO [Point]
generateFourSquarePoints dim lines = do 
  firstPoint <- generatePoint dim `suchThat` isContainedInSomeLine lines
  secondPoint <- generatePoint dim `suchThat` (\p -> isContainedInSomeLine lines p && shouldBeBelow p firstPoint)
  thirdPoint <- generatePoint dim `suchThat` (\p -> isContainedInSomeLine lines p && shouldOnRight p secondPoint && inSameLine p secondPoint)
  fourthPoint <- generatePoint dim `suchThat` (\p -> isContainedInSomeLine lines p && shouldBeAbove p thirdPoint && inSameLine p firstPoint)
  return [firstPoint, secondPoint, thirdPoint, fourthPoint]


generateSquares :: DimensionFrame -> Int -> Lines -> IO FilledSquares
generateSquares dim number lines =
  replicate number
    $   generate
    $   FilledSquare
    <$> generateFourSquarePoints dim lines
    <*> generateColor


generateMondrian :: DimensionFrame -> IO MondrianPainting
generateMondrian dim = do
  numberVerticalLines   <- generate $ choose (3, 10)
  numberHorizontalLines <- generate $ choose (4, 7)
  numberFilledSquares   <- generate $ choose (3, 5)
  vLines                <- generateVerticalLines dim numberVerticalLines
  hLines                <- generateHorizontalLines dim numberHorizontalLines
  let lines = vLines <> hLines
  squares <- generateSquares dim numberFilledSquares lines
  return $ MondrianPainting { _mpLines = lines, _mpFilledSquares = squares }




