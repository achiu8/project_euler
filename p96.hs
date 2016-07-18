import Control.Monad
import Data.Array
import Data.List
import Data.List.Split

type Digit = Char
type Square = (Char, Char)
type Unit = [Square]
type Grid = Array Square [Digit]

cross :: String -> String -> [Square]
cross as bs = [ (a,b) | a <- as, b <- bs ]

digits = ['1'..'9']
rows = ['A'..'I']
cols = digits
squares = cross rows cols
box = (head squares, last squares)

unitlist = [ cross rows [c] | c <- cols ]
        ++ [ cross [r] cols | r <- rows ]
        ++ [ cross rs cs | rs <- ["ABC","DEF","GHI"], cs <- ["123","456","789"] ]

units :: Array Square [Unit]
units = array box [ (s, containing s) | s <- squares ]
  where containing s = filter (elem s) unitlist

peers :: Array Square [Square]
peers = array box [ (s, peers' s) | s <- squares ]
  where peers' s = delete s $ foldl1 union (units ! s)

parseGrid :: String -> Maybe Grid
parseGrid g = foldM assign allValues (zip squares g)
  where allValues = array box [ (s,digits) | s <- squares ]

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if d `elem` digits
                   then do
                     let toDump = delete d (g ! s)
                     foldM eliminate g (zip (repeat s) toDump)
                   else return g

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) =
  let cell = g ! s in
  if d `notElem` cell then return g
    else do
      let newCell = delete d cell
          newGrid = g // [(s,newCell)]
      newGrid' <- case newCell of
                    []   -> Nothing
                    [d'] -> foldM eliminate newGrid (zip (peers ! s) (repeat d'))
                    _    -> return newGrid
      foldM (locate d) newGrid' (units ! s) 

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (g !)) u of
                 []  -> Nothing
                 [s] -> assign g (s,d)
                 _   -> return g

search :: Grid -> Maybe Grid
search g = case [ (l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1 ] of
             [] -> return g
             ls -> do let (_,(s,ds)) = minimum ls
                      msum [ assign g (s,d) >>= search | d <- ds ]

solve :: String -> Maybe Grid
solve str = parseGrid str >>= search

display :: Grid -> IO ()
display g = do
  let rows = groupRows . concat $ elems g
  mapM_ putStrLn rows

groupRows :: String -> [String]
groupRows [] = []
groupRows str = take 9 str : groupRows (drop 9 str)

firstThree :: Grid -> Int
firstThree = read . concat . take 3 . elems
  
test :: IO ()
test = do
  print $ length squares == 81
  print $ length unitlist == 27
  print $ all (\s -> length (units ! s) == 3) squares
  print $ all (\s -> length (peers ! s) == 20) squares
  print $ units ! ('C','2') == [cross rows "2", cross "C" cols, cross "ABC" "123"]
  print $ peers ! ('C','2') == cross "ABDEFGHI" "2" ++ cross "C" "13456789" ++ cross "AB" "13"

solveAndPrint :: String -> IO ()
solveAndPrint g = do
  let Just solved = solve g
  display solved

main = do
  contents <- readFile "p96.txt"
  let grids = filter (not . null) $ splitOn "\n" contents
  print $ foldl' (\acc (Just g) -> acc + (firstThree g)) 0 (fmap solve grids)
