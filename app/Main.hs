module Main where

import Data.Foldable (traverse_)
import MINQ

-- Defines a row type for top-level movie metadata.
data Movie
  = Movie
      { movieId :: Int
      , movieGenres :: [String]
      , movieYear :: Int
      , movieTitle :: String
      }
  deriving Show

-- Defines a row type for movie trailers.
data Trailer
  = Trailer
      { trailerId :: Int
      , trailerMovieId :: Int -- FK to Movie
      , trailerURL :: String
      , trailerActive :: Bool
      }
  deriving Show

-- Select action (or science fiction) movies.
actionMovies :: [Movie] -> [Movie]
actionMovies movies =
  runMINQ $
    MINQ (select_ id) -- select *
      movies
      (where_ $ (elem "Action" `or_` elem "Sci-Fi") . movieGenres)

-- Select id and title of action movies released in the 1980s.
actionMovies80s :: [Movie] -> [(Int, String)]
actionMovies80s movies =
  runMINQ $
    MINQ (select_ $ \r -> (movieId r, movieTitle r))
      (actionMovies movies)
      (where_ $ (`elem` [1980.. 1989]) . movieYear)

-- Select movie name and trailer URL of movies that have at least one active trailer.
movieTrailers :: [Movie] -> [Trailer] -> [(String, String)]
movieTrailers movies trailers =
  runMINQ $
    MINQ (select_ $ \(m, t) -> (movieTitle m, trailerURL t))
      (join_ movies movieId trailers trailerMovieId)
      (where_ $ (== True) . trailerActive . snd)

-- Sample movie table.
movieTable :: [Movie]
movieTable =
  [ Movie 11 ["Action"] 1988 "Die Hard"
  , Movie 52 ["Action"] 1987 "Top Gun"
  , Movie 24 ["Comedy"] 1987 "Space Balls"
  , Movie 73 ["Action"] 2020 "Top Gun: Maverick"
  , Movie 35 ["Action"] 1987 "Lethal Weapon"
  , Movie 46 ["Sci-Fi"] 1987 "Predator"
  , Movie 67 ["Sci-Fi"] 1990 "Predator 2"
  , Movie 51 ["Action"] 1990 "Die Hard 2"
  ]

-- Sample trailer table.
trailerTable :: [Trailer]
trailerTable =
  [ Trailer 1 11 "https://www.youtube.com/watch?v=2TQ-pOvI6Xo" True
  , Trailer 2 73 "https://www.youtube.com/watch?v=qSqVVswa420" True
  , Trailer 3 24 "https://www.youtube.com/watch?v=kGIM_yNzeUo" False
  ]

-- Run various example MINQ queries.
main :: IO ()
main = do
  putStrLn "\n80s Action Movies: "
  traverse_ print (actionMovies80s movieTable)
  putStrLn "\nMovies with active trailers:"
  traverse_ print (movieTrailers movieTable trailerTable)
