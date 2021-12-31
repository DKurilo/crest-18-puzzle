{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( printAllPossible,
  )
where

import Control.DeepSeq
import Control.Monad (liftM2, mapM)
import Control.Monad.Par
import Data.List (delete)
import qualified Data.Set as S
import Data.Vector (Vector (..), fromList, (!))
import Debug.Trace
import GHC.Generics (Generic)

data Place = E | F deriving (Eq, Show, Generic, NFData)

newtype Crest = Crest (Vector (Vector (Vector Place))) deriving (Eq, Show, Generic, NFData)

data Axis = X | Y | Z deriving (Eq, Show, Generic, NFData)

data Inverse = N | I deriving (Eq, Show, Generic, NFData)

data Angle = A0 | A1 | A2 | A3 deriving (Eq, Show, Generic, NFData)

data Part = Part Int (Vector (Vector (Vector Place))) deriving (Eq, Show, Generic, NFData)

data Placed = Placed Part Axis Inverse Angle deriving (Eq, Generic, NFData)

show' :: Placed -> String
show' (Placed (Part n _) ax i a) = show n ++ " " ++ show ax ++ " " ++ show i ++ " " ++ show a

instance Show Placed where
  show (Placed (Part n _) Z N A0) = show (n + 8) <> " " <> "A3"
  show (Placed (Part n _) Z N A1) = show (n + 8) <> " " <> "A2"
  show (Placed (Part n _) Z N A2) = show (n + 8) <> " " <> "A1"
  show (Placed (Part n _) Z N A3) = show (n + 8) <> " " <> "A0"
  show (Placed (Part n _) Z I A0) = show n <> " " <> "A1"
  show (Placed (Part n _) Z I A1) = show n <> " " <> "A2"
  show (Placed (Part n _) Z I A2) = show n <> " " <> "A3"
  show (Placed (Part n _) Z I A3) = show n <> " " <> "A0"
  show (Placed (Part n _) Y N A0) = show n <> " " <> "A0"
  show (Placed (Part n _) Y N A1) = show n <> " " <> "A1"
  show (Placed (Part n _) Y N A2) = show n <> " " <> "A2"
  show (Placed (Part n _) Y N A3) = show n <> " " <> "A3"
  show (Placed (Part n _) Y I A0) = show (n + 8) <> " " <> "A2"
  show (Placed (Part n _) Y I A1) = show (n + 8) <> " " <> "A1"
  show (Placed (Part n _) Y I A2) = show (n + 8) <> " " <> "A0"
  show (Placed (Part n _) Y I A3) = show (n + 8) <> " " <> "A3"
  show (Placed (Part n _) X N A0) = show (n + 8) <> " " <> "A0"
  show (Placed (Part n _) X N A1) = show (n + 8) <> " " <> "A3"
  show (Placed (Part n _) X N A2) = show (n + 8) <> " " <> "A2"
  show (Placed (Part n _) X N A3) = show (n + 8) <> " " <> "A1"
  show (Placed (Part n _) X I A0) = show n <> " " <> "A2"
  show (Placed (Part n _) X I A1) = show n <> " " <> "A3"
  show (Placed (Part n _) X I A2) = show n <> " " <> "A0"
  show (Placed (Part n _) X I A3) = show n <> " " <> "A1"

data State = State Crest [(Int, Int, Int)] [Part] [(Placed, Int, Int, Int)] deriving (Eq, Generic, NFData)

instance Show State where
  show (State _ _ _ ps) =
    "Top\n"
      <> getForCoords 2 1 0
      <> "\t"
      <> getForCoords 2 3 0
      <> "\t"
      <> getForCoords 2 5 0
      <> "\n"
      <> getForCoords 4 1 0
      <> "\t"
      <> getForCoords 4 3 0
      <> "\t"
      <> getForCoords 4 5 0
      <> "\n"
      <> "Left\n"
      <> getForCoords 1 0 4
      <> "\t"
      <> getForCoords 3 0 4
      <> "\t"
      <> getForCoords 5 0 4
      <> "\n"
      <> getForCoords 1 0 2
      <> "\t"
      <> getForCoords 3 0 2
      <> "\t"
      <> getForCoords 5 0 2
      <> "\n"
      <> "Right\n"
      <> getForCoords 0 2 5
      <> "\t"
      <> getForCoords 0 4 5
      <> "\n"
      <> getForCoords 0 2 3
      <> "\t"
      <> getForCoords 0 4 3
      <> "\n"
      <> getForCoords 0 2 1
      <> "\t"
      <> getForCoords 0 4 1
      <> "\n"
      <> "End"
    where
      getForCoords x y z = if null ps' then "-----" else (show . (\(p, _, _, _) -> p) . head) ps'
        where
          ps' = filter (\(_, x', y', z') -> x == x' && y == y' && z == z') ps

partX = 1

partY = 7

partZ = 1

crestX = 7

crestY = 7

crestZ = 7

toCoords = fromList . map (fromList . map fromList)

toPart :: Int -> [[[Place]]] -> Part
toPart n = Part n . toCoords

toCrest :: [[[Place]]] -> Crest
toCrest = Crest . toCoords

part17 =
  toPart
    17
    [ [[F, F], [F, F], [F, E], [F, E], [F, E], [F, E], [F, F], [F, F]],
      [[F, F], [E, E], [E, E], [E, E], [E, E], [E, E], [E, E], [F, F]]
    ]

allParts :: [Part]
allParts =
  [ toPart
      7
      [ [[F, F], [F, F], [F, F], [F, E], [F, F], [E, F], [E, F], [F, F]],
        [[F, F], [E, E], [E, E], [E, E], [E, E], [E, F], [E, F], [F, F]]
      ],
    toPart
      6
      [ [[F, F], [F, F], [E, E], [E, E], [E, E], [E, E], [E, E], [F, F]],
        [[F, F], [F, F], [F, F], [F, F], [F, E], [F, E], [F, E], [F, F]]
      ],
    toPart
      3
      [ [[F, F], [F, F], [E, E], [E, E], [F, F], [F, F], [F, F], [F, F]],
        [[F, F], [F, F], [F, F], [F, E], [F, F], [F, F], [F, F], [F, F]]
      ],
    toPart
      4
      [ [[F, F], [F, F], [F, E], [F, E], [F, F], [F, E], [F, F], [F, F]],
        [[F, F], [F, F], [E, E], [E, E], [E, E], [E, E], [F, F], [F, F]]
      ],
    toPart
      1
      [ [[F, F], [F, F], [F, F], [F, F], [F, E], [F, E], [F, F], [F, F]],
        [[F, F], [E, E], [E, E], [E, E], [E, E], [E, E], [F, F], [F, F]]
      ],
    toPart
      8
      [ [[F, F], [F, F], [F, E], [F, E], [F, F], [F, E], [F, F], [F, F]],
        [[F, F], [E, E], [E, E], [E, E], [E, E], [F, E], [F, F], [F, F]]
      ],
    toPart
      2
      [ [[F, F], [E, F], [E, E], [F, F], [E, E], [E, E], [E, F], [F, F]],
        [[F, F], [E, F], [F, F], [F, F], [F, F], [F, F], [E, F], [F, F]]
      ],
    toPart
      5
      [ [[F, F], [F, E], [F, F], [F, E], [F, F], [F, F], [F, E], [F, F]],
        [[F, F], [F, E], [E, E], [E, E], [F, F], [E, E], [F, E], [F, F]]
      ]
  ]
    ++ replicate 10 part17

perimeter :: S.Set (Int, Int, Int)
perimeter =
  S.fromList $
    [(x, y, z) | x <- [2 .. 5], y <- [1 .. 6], z <- [0, 7]] ++ [(x, y, z) | x <- [2 .. 5], y <- [1, 6], z <- [1, 6]]
      ++ [(x, y, z) | z <- [2 .. 5], x <- [1 .. 6], y <- [0, 7]]
      ++ [(x, y, z) | z <- [2 .. 5], x <- [1, 6], y <- [1, 6]]
      ++ [(x, y, z) | y <- [2 .. 5], z <- [1 .. 6], x <- [0, 7]]
      ++ [(x, y, z) | y <- [2 .. 5], z <- [1, 6], x <- [1, 6]]

isPerimeter :: Int -> Int -> Int -> Bool
isPerimeter x y z = (x, y, z) `S.member` perimeter

ratate90ClockwiseY :: Part -> Part
ratate90ClockwiseY (Part n ps) =
  toPart
    n
    [ [ [ps ! (partX - x) ! y ! z | x <- [0 .. partX]]
        | y <- [0 .. partY]
      ]
      | z <- [0 .. partZ]
    ]

setAngle :: Angle -> Part -> Part
setAngle A0 = id
setAngle A1 = setAngle A0 . ratate90ClockwiseY
setAngle A2 = setAngle A1 . ratate90ClockwiseY
setAngle A3 = setAngle A2 . ratate90ClockwiseY

setInverse :: Inverse -> Part -> Part
setInverse N p = p
setInverse I (Part n ps) =
  toPart
    n
    [ [ [ps ! x ! (partY - y) ! (partZ - z) | z <- [0 .. partZ]]
        | y <- [0 .. partY]
      ]
      | x <- [0 .. partX]
    ]

setAxis :: Axis -> Part -> Part
setAxis X (Part n ps) =
  toPart
    n
    [ [ [ps ! (partX - x) ! y ! z | z <- [0 .. partZ]]
        | x <- [0 .. partX]
      ]
      | y <- [0 .. partY]
    ]
setAxis Y p = p
setAxis Z (Part n ps) =
  toPart
    n
    [ [ [ps ! x ! y ! (partZ - z) | y <- [0 .. partY]]
        | z <- [0 .. partZ]
      ]
      | x <- [0 .. partX]
    ]

possiblePlaced :: Axis -> Part -> [Placed]
possiblePlaced ax p = [Placed ((setAxis ax . setInverse i . setAngle a) p) ax i a | a <- [A0, A1, A2, A3], i <- [N, I]]

line0 = [F, F, F, F, F, F, F, F]

line4 = [F, F, E, E, E, E, F, F]

line6 = [F, E, E, E, E, E, E, F]

line8 = [E, E, E, E, E, E, E, E]

crest18 :: Crest
crest18 =
  toCrest
    [ [line0, line0, line6, line6, line6, line6, line0, line0],
      [line4, line4, line6, line6, line6, line6, line4, line4],
      [line4, line8, line8, line8, line8, line8, line8, line4],
      [line4, line8, line8, line8, line8, line8, line8, line4],
      [line4, line8, line8, line8, line8, line8, line8, line4],
      [line4, line8, line8, line8, line8, line8, line8, line4],
      [line4, line4, line6, line6, line6, line6, line4, line4],
      [line0, line0, line6, line6, line6, line6, line0, line0]
    ]

allPositions :: [(Int, Int, Int)]
allPositions =
  [ (3, 0, 2),
    (3, 0, 4),
    (0, 2, 3),
    (0, 4, 3),
    (2, 3, 0),
    (4, 3, 0),
    (2, 1, 0),
    (2, 5, 0),
    (4, 1, 0),
    (4, 5, 0),
    (0, 2, 1),
    (0, 2, 5),
    (0, 4, 1),
    (0, 4, 5),
    (1, 0, 2),
    (5, 0, 2),
    (1, 0, 4),
    (5, 0, 4)
  ]

initialState :: State
initialState = State crest18 allPositions allParts []

tryToPlace :: Part -> Int -> Int -> Int -> Crest -> Maybe Crest
tryToPlace (Part _ p) x y z (Crest crest)
  | x + maxX > crestX || y + maxY > crestY || z + maxZ > crestZ = Nothing
  | canPlace =
    Just . toCrest $
      [ [ [place i j k | k <- [0 .. crestZ]]
          | j <- [0 .. crestY]
        ]
        | i <- [0 .. crestX]
      ]
  | otherwise = Nothing
  where
    maxX = length p - 1
    maxY = length (p ! 0) - 1
    maxZ = length (p ! 0 ! 0) - 1
    canPlace =
      and
        [ (p ! i ! j ! k == E || crest ! x' ! y' ! z' == E)
            && (not per || per && (p ! i ! j ! k /= E || crest ! x' ! y' ! z' /= E))
          | i <- [0 .. maxX],
            let x' = i + x,
            j <- [0 .. maxY],
            let y' = j + y,
            k <- [0 .. maxZ],
            let z' = k + z,
            let per = isPerimeter x' y' z'
        ]
    place i j k
      | i < x || i > x + maxX || j < y || j > y + maxY || k < z || k > z + maxZ = crest ! i ! j ! k
      | p ! (i - x) ! (j - y) ! (k - z) == F = F
      | otherwise = crest ! i ! j ! k

isEdge :: (Int, Int, Int) -> Bool
isEdge (x, y, z) = edge x || edge y || edge z
  where
    edge i = i == 1 || i == 5

tryPossible :: State -> (Int, Int, Int) -> Placed -> Par [State]
tryPossible (State crest ps parts placeds) (x, y, z) (Placed part@(Part n _) _ _ _)
  | n == 6 && isEdge (z, y, z) = return [] -- part 6 should be inner
  | otherwise = case tryToPlace part x y z crest of
    Just crest' -> findAllPossible (State crest' ps parts placeds)
    _ -> return []

findAllPossible :: State -> Par [State]
findAllPossible s@(State _ _ [] _) = trace (show s) $ return [s]
findAllPossible s@(State _ [] _ _) = trace (show s) $ return [s]
findAllPossible (State crest ps (part@(Part 7 _) : parts) placeds) | (3, 0, 2) `elem` ps = do
  let ps' = filter (/= (3, 0, 2)) ps
      go placed = tryPossible (State crest ps' parts ((placed, 3, 0, 2) : placeds)) (3, 0, 2) placed
      possiblePlaced' = [Placed ((setAxis Y . setAngle a) part) Y N a | a <- [A0, A1, A2, A3]]
  rs <- mapM (spawn . go) possiblePlaced'
  concat <$> mapM get rs
findAllPossible (State crest (p@(x, y, z) : ps) parts placeds) =
  concat
    <$> mapM
      ( \part -> do
          let ax
                | x == 0 = X
                | y == 0 = Y
                | z == 0 = Z
                | otherwise = error "Wrong position"
              parts' = delete part parts
              go placed = tryPossible (State crest ps parts' ((placed, x, y, z) : placeds)) p placed
          if p == (3, 0, 4)
            then do
              rs <- (mapM (spawn . go) . possiblePlaced ax) part
              concat <$> mapM get rs
            else concat <$> (mapM go . possiblePlaced ax) part
      )
      (partsNot17 ++ parts17)
  where
    partsNot17 = filter (\(Part n _) -> n /= 17) parts
    parts17 = take 1 . filter (\(Part n _) -> n == 17) $ parts

printAllPossible :: IO ()
printAllPossible =
  ( mapM_ (\(i, s) -> putStrLn ("Variant: " <> show i <> "\n" <> show s))
      . zip [1 ..]
      . runPar
      . findAllPossible
  )
    initialState
