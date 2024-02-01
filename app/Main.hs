{-# LANGUAGE LambdaCase, TemplateHaskell, BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Debug.Trace

import Data.Array.Base
import qualified Data.List as List (genericIndex, isSuffixOf)
import qualified Data.List.Split as Split (chunksOf)
import Data.Maybe
import Data.Word
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified System.ProgressBar as Bar

import Control.Lens

import Codec.Picture.Bitmap
import Codec.Picture.Types

import qualified Data.ByteString as ByteString
import qualified Graphics.Gloss as Gloss

---[Defines]---------------------------------------------
extendingAmount :: Int
extendingAmount = 50

emptyElement :: Word8
emptyElement = 0

imageDir :: String
imageDir = "./imgs/"

---[Rotation]---------------------------------------------
data Direction
  = L
  | R
  | U
  | D
  deriving (Eq, Show, Read)

type Rotation = Direction -> Direction

rLeft :: Rotation
rLeft =
  \case
    L -> D
    D -> R
    R -> U
    U -> L

rBack :: Rotation
rBack =
  \case
    L -> R
    R -> L
    D -> U
    U -> D

rRight :: Rotation
rRight = rLeft . rBack

rNot :: Rotation
rNot = id

dirAsVec :: Direction -> Point
dirAsVec d =
  uncurry Point
    $ case d of
        L -> (-1, 0)
        R -> (1, 0)
        U -> (0, 1)
        D -> (0, -1)

---[Point]----------------------------------------------------
data Point = Point
  { _x :: Int
  , _y :: Int
  } deriving (Eq, Show, Read)

$(makeLenses ''Point)

instance Num Point where
  Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
  Point x1 y1 * Point x2 y2 = Point (x1 * x2) (y1 * y2)
  negate (Point x1 y1) = Point (negate x1) (negate y1)
  abs (Point x1 y1) = Point (abs x1) (abs y1)
  fromInteger k = Point (fromIntegral k) 0
  signum = id

---[Extensible Array]----------------------------------------------------
type ExtArr = UArray Int Word8

showExtArr :: ExtArr -> String
showExtArr = unlines . map show . toLists

extSize :: ExtArr -> Int
extSize = round . sqrt . fromIntegral . succ . snd . bounds

fromCoord :: ExtArr -> Point -> Int
fromCoord extarr (Point x1 y1) = x1 + (extSize extarr * y1)

writeArr :: Point -> Word8 -> ExtArr -> ExtArr
writeArr p w a = a // [(fromCoord a p, w)]

isOutOfBounds :: ExtArr -> Point -> Bool
isOutOfBounds arr (Point x1 y1) =
  x1 < 0 || y1 < 0 || x1 >= extSize arr || y1 >= extSize arr

(!?) :: ExtArr -> Point -> Maybe Word8
extarr !? p
  | isOutOfBounds extarr p = Nothing
  | otherwise = Just $ extarr ! fromCoord extarr p

toLists :: ExtArr -> [[Word8]]
toLists extarr = Split.chunksOf (extSize extarr) (elems extarr)

fromLists :: [[Word8]] -> ExtArr
fromLists ls = listArray (0, length ls ^ 2 - 1) (concat ls)

extendArr :: ExtArr -> Int -> ExtArr
extendArr ar amount = fromLists (top <> core <> top)
  where
    newWid = amount * 2 + extSize ar
    top = replicate amount (replicate newWid emptyElement)
    core =
      let side = replicate amount emptyElement
       in map (\a -> side <> a <> side) (toLists ar)

mkArr :: Int -> ExtArr
mkArr sz = unsafeArray (0, sz2) (map (, emptyElement) [0 .. sz2])
  where
    sz2 = sz * sz - 1

---[Transitions]----------------------------------------------------
type StateId = Int

type Input = Word8

type WriteAction = ExtArr -> ExtArr

type TrSet = [((Input, StateId), (Point -> WriteAction, Ant -> Ant))]

data Ant = Ant
  { _genRule :: String
  , _facing :: Direction
  , _stateid :: StateId
  , _point :: Point
  , _halt :: Bool
  , _trset :: TrSet
  }

$(makeLenses ''Ant)

instance Show Ant where
  show = view genRule

antStep :: Ant -> Ant
antStep = antTranslate =<< (dirAsVec . view facing)

killAnt :: Ant -> Ant
killAnt = set halt True

antFlip :: Rotation -> Ant -> Ant
antFlip = over facing

antTranslate :: Point -> Ant -> Ant
antTranslate p = over point (+ p)

antPerform :: ExtArr -> Ant -> (WriteAction, Ant)
antPerform arr a =
  let inp = arr ! fromCoord arr (a ^. point)
      (pwriter, stepNflip) =
        (const id, killAnt) `fromMaybe` lookup (inp, a ^. stateid) (a ^. trset)
   in (pwriter (a ^. point), stepNflip a)

mkLangtonTrs :: [Rotation] -> TrSet
mkLangtonTrs = run 0
  where
    buildTr r rd wrt = ((rd, 0), (flip writeArr wrt, antStep . antFlip r))
    run i =
      \case
        [] -> []
        [r] -> [buildTr r i 0]
        (r:rs) -> buildTr r i (i + 1) : run (i + 1) rs

mkTurmiteTrs :: [[(Word8, Rotation, StateId)]] -> TrSet
mkTurmiteTrs =
  concatMap (\(stf, rls) -> genTr stf <$> zip [0 ..] rls) . zip [0 ..]
  where
    genTr st (n, (w, r, s)) =
      ((n, st), (flip writeArr w, set stateid s . antStep . antFlip r))

---[Ant Parser]------------------------------------------------------
parseRotation :: Parser Rotation
parseRotation =
  choice
    [ char 'R' >> return rRight
    , char 'L' >> return rLeft
    , char 'N' >> return rNot
    , char 'B' >> return rBack
    ]

parsePoint :: Parser Point
parsePoint = do
  char '('
  x <- many1 digit
  char ','
  y <- many1 digit
  char ')'
  return (read x `Point` read y)

trimSuffix :: String -> String -> String
trimSuffix suffix str
  | suffix `List.isSuffixOf` str = take (length str - length suffix) str
  | otherwise = str

parseTrips :: Parser [(Word8, Rotation, StateId)]
parseTrips = do
  char '['
  trips <- many parseTrip
  char ']'
  pure trips

parseTrip :: ParsecT String () Identity (Word8, Rotation, StateId)
parseTrip = do
  char '{'
  d <- digit
  r <- parseRotation
  s <- digit
  char '}'
  return (read [d], r, read [s])

parseAnt :: Parser Ant
parseAnt = do
  total <- getInput
  p <- parsePoint
  quasiAnt <-
    choice [parseTurmite, parseLangton]
  rest <- getInput
  let rule = trimSuffix rest total
  return $ quasiAnt rule p

parseAnts :: Parser [Ant]
parseAnts = do
  ants <- many parseAnt
  eof
  return ants

parseAnts' :: String -> [Ant]
parseAnts' s =
  case parse parseAnts "" s of
    Left err -> error $ show err
    Right as -> as

parseTurmite :: Parser (String -> Point -> Ant)
parseTurmite = do
  char '['
  trset <- mkTurmiteTrs <$> many parseTrips
  char ']'
  return (\r p -> Ant r U 0 p False trset)

parseLangton :: Parser (String -> Point -> Ant)
parseLangton = do
  trset <- mkLangtonTrs <$> many parseRotation
  return (\r p -> Ant r U 0 p False trset)

---[Colony]----------------------------------------------------------
data Colony = Colony
  { _ants :: [Ant]
  , _tape :: ExtArr
  , _steps :: Int
  }

$(makeLenses ''Colony)

extendByAnts :: Colony -> Colony
extendByAnts c@(Colony as t _)
  | any (isOutOfBounds t . view point) as =
    over ants (map (antTranslate (Point extendingAmount extendingAmount)))
      . over tape (`extendArr` extendingAmount)
      $ c
  | otherwise = c

colStep :: Colony -> Colony
colStep c =
  over steps succ
    . over ants (filter (not . view halt))
    . over tape (foldl1 (.) actions)
    . set ants newAnts
    $ c1
  where
    c1 = extendByAnts c
    (actions, newAnts) = unzip $ map (antPerform (c1 ^. tape)) (c1 ^. ants)

pureColRun :: Int -> Colony -> Colony
pureColRun n c
  | n <= 0 || null (c ^. ants) = c
  | otherwise =
    let !stp = colStep c
     in pureColRun (n - 1) stp

---[Colors]------------------------------------------
type ColorPalette = Word8 -> PixelRGB8

tupleToPix :: (Pixel8, Pixel8, Pixel8) -> PixelRGB8
tupleToPix (r, g, b) = PixelRGB8 r g b

pixelBlack :: PixelRGB8
pixelBlack = tupleToPix (0, 0, 0)

black :: ColorPalette
black = const pixelBlack

dracula :: ColorPalette
dracula =
  List.genericIndex
    $ tupleToPix
        <$> cycle
              [ (40, 42, 54)
              , (255, 184, 108)
              , (248, 248, 242)
              , (68, 71, 90)
              , (139, 233, 253)
              , (189, 147, 249)
              , (80, 250, 123)
              , (255, 121, 198)
              , (255, 85, 85)
              , (241, 250, 140)
              ]

mono :: ColorPalette
mono = List.genericIndex $ tupleToPix . (\x -> (x, x, x)) <$> cycle [0,2 ..]

---[Images]-----------------------------------------------------
arrToImg :: ColorPalette -> ExtArr -> Image PixelRGB8
arrToImg cpl arr = generateImage getPx (extSize arr) (extSize arr)
  where
    getPx x y = cpl (arr ! fromCoord arr (Point x y))

writeBMP :: ColorPalette -> FilePath -> ExtArr -> IO ()
writeBMP cpl path arr =
  writeBitmap (imageDir ++ path ++ ".bmp") (arrToImg cpl arr)

---[Gloss]------------------------------------------------------
toGlossPic :: ColorPalette -> ExtArr -> Gloss.Picture
toGlossPic cpl arr =
  Gloss.bitmap $ Gloss.bitmapDataOfByteString sz sz format str False
  where
    sz = extSize arr
    format = Gloss.BitmapFormat Gloss.BottomToTop Gloss.PxRGBA
    str = ByteString.pack $ (pixToWords . cpl) `concatMap` concat (toLists arr)
    pixToWords (PixelRGB8 r g b) = [r, g, b, 255]

---[Runners]----------------------------------------------------
type Runner = Colony -> Int -> IO Colony

runnerBar :: Runner
runnerBar col0 total = do
  pbar <- Bar.newProgressBar Bar.defStyle 1 (Bar.Progress 0 100 ())
  let updateBar lim =
        let pct =
              truncate $ 100 * fromIntegral (total - lim) / fromIntegral total
         in Bar.updateProgress pbar (const $ Bar.Progress pct 100 ())
  run total col0 updateBar
  where
    stepInterval = round $ 0.05 * fromIntegral total
    run lim col1 upd
      | lim <= 0 = return col1
      | otherwise = do
        let !col2 = pureColRun stepInterval col1
        upd lim
        run (lim - stepInterval) col2 upd

runnerGloss :: ColorPalette -> Colony -> Int -> IO ()
runnerGloss cpl col0 total =
  Gloss.play
    window
    background
    60
    col0
    (toGlossPic cpl . view tape)
    (const id)
    (const $ pureColRun stepInterval)
  where
    background = Gloss.makeColor 0 0 0 255
    stepInterval = round $ 0.0001 * fromIntegral total
    size = 300
    window = Gloss.InWindow "Cum" (size, size) (10, 10)

---------------------------------------------------------------
run :: Int -> String -> IO ()
run lim rule = do
  let as = parseAnts' rule
      col = Colony as (mkArr 100) 0
  col2 <- runnerBar col lim
  writeBMP dracula rule (view tape col2)

main :: IO ()
main = do
  args <- getArgs
  print args
  let lim = read $ args !! 0
      rule = args !! 1
  run lim rule


[[{0L1}{1R1}][{1N0}{1N1}]]
[[{0N1}{0L1}][{1R0}{0N1}]]
