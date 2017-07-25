module Main where

import Protolude hiding (readFile, writeFile)
import Data.ByteString.Lazy (readFile, writeFile)
import Prelude (tail, lookup)
import Control.Lens
import Data.Text (pack)
import Data.Aeson
import System.Random.MWC
import Options.Applicative
import Data.Time.Clock

data GameFrame = GameFrame [(Text, Maybe Text)] UTCTime
  deriving (Show, Eq, Generic)

instance ToJSON GameFrame

instance FromJSON GameFrame

type Game = [GameFrame]

toMap :: Game -> [(Text, Maybe Text)]
toMap g = concat $ map (\(GameFrame c _) -> c) g

status :: Game -> [(Text, Text)]
status g = statusHelper [] (toMap g)
  where statusHelper visited [] = []
        statusHelper visited ((player, Just target):rest) = if elem player visited
          then statusHelper visited rest
          else (player, target):(statusHelper (player:visited) rest)
        statusHelper visited ((player, Nothing):rest) = if elem player visited
          then statusHelper visited rest
          else (player, "TERMINATED"):(statusHelper (player:visited) rest)
  
generateGame :: [Text] -> IO Game
generateGame names = do
  gen <- createSystemRandom
  targets <- shuffle gen names
  time <- getCurrentTime
  return $ [GameFrame (zip targets (map Just $ tail $ cycle targets)) time]

insertPos :: [a] -> a -> Int -> [a]
insertPos [] y _ = [y]
insertPos (x:xs) y n = if n == 0 then y:x:xs else x:(insertPos xs y (n - 1))

randInsert :: GenIO -> a -> [a] -> IO [a]
randInsert gen x xs = do
  idx <- uniformR (0, (length xs) - 1) gen
  return $ insertPos xs x idx

shuffle :: GenIO -> [a] -> IO [a]
shuffle _ [] = return []
shuffle gen (x:xs) = do
  prev <- shuffle gen xs
  randInsert gen x prev

data Options = 
  CreateOptions
    { input :: FilePath
    , output :: FilePath
    } |
  KillOptions
    { game :: FilePath
    , player :: Text
    } |
  QueryOptions
    { game :: FilePath
    , player :: Text
    } |
  UndoOptions
    { game :: FilePath
    } |
  ShowOptions
    { game :: FilePath
    , targets :: Bool
    }
  deriving (Eq, Show)

kill :: UTCTime -> Text -> Game -> Maybe (Game, Text, Text)
kill time p g = do
  let map = toMap g
  killedM <- lookup p map
  killed <- killedM
  nextM <- lookup killed map
  next <- nextM
  return $ ((GameFrame [(p, Just next), (killed, Nothing)] time):g, killed, next)

cmdLine :: Parser Options
cmdLine =
  switch
      ( long "create"
     <> short 'c'
     <> help "Whether to create the game" ) *>
        (
         CreateOptions
           <$> strOption
           ( long "input"
             <> short 'i'
             <> help "Where the list of names is" )
           <*> strOption
           ( long "output"
             <> short 'o'
             <> help "Where to output the game" )
        )
  <|> switch
      ( long "kill"
     <> short 'k'
     <> help "Whether to kill a player" ) *>
        (
         KillOptions
           <$> strOption
             ( long "game"
            <> short 'g'
            <> help "Where the currently running game is" )
           <*> (pack <$> strOption
             ( long "player"
            <> short 'p'
            <> help "Who just killed their target" ))
        )
  <|> switch
      ( long "query"
     <> short 'q'
     <> help "Query the database" ) *>
        (
         QueryOptions
           <$> strOption
             ( long "game"
            <> short 'g'
            <> help "Where the currently running game is" )
           <*> (pack <$> strOption
             ( long "player"
            <> short 'p'
            <> help "The player to query" ))
        )
  <|> switch
      ( long "undo"
     <> short 'u'
     <> help "Undo the previous kill" ) *>
        (
         UndoOptions
          <$> strOption
            ( long "game"
           <> short 'g'
           <> help "Where the currently running game is" )
        )
  <|> switch
      ( long "show"
     <> short 's'
     <> help "Show the current state of the game" ) *>
        (
         ShowOptions
          <$> strOption
            ( long "game"
           <> short 'g'
           <> help "Where the currently running game is" )
          <*> switch
                ( long "alive"
               <> short 'a'
               <> help "Whether to show alive or not" )
        )

main :: IO ()
main = do
  options <- execParser opts
  case options of
    (CreateOptions i o) -> do
      namesM <- decode <$> readFile i :: IO (Maybe [Text])
      case namesM of
        (Just names) -> do
          game <- generateGame names
          writeFile o $ encode game
          putText "Game successfully generated"
        Nothing -> putText "Could not decode input file"  
    (KillOptions gfile p) -> do
      g <- decode <$> readFile gfile
      time <- getCurrentTime
      case g >>= kill time p of
        (Just (newg, killed, next)) -> do
          writeFile gfile $ encode newg
          putText $ p <> " killed " <> killed <> ".\n" <> "New target: " <> next
        Nothing -> do
          putText "Player not found"
    (QueryOptions gfile p) -> do
      g <- decode <$> readFile gfile
      case g >>= lookup p . toMap of
        (Just found) -> do
          case found of
            (Just target) -> putText $ "Target: " <> target
            Nothing -> putText "TERMINATED"
        Nothing -> putText "404 Assassin not found"
    (UndoOptions gfile) -> do
      g <- decode <$> readFile gfile :: IO (Maybe Game)
      case g of
        Just (_:frames) -> do
          writeFile gfile $ encode frames
          putText "Previous kill undone"
        _ -> putText "Undo unsuccessful"
    (ShowOptions gfile alive) -> do
      (Just game) <- decode <$> readFile gfile
      let s = status game
      let s' = if alive then map (over _2 (\target -> if target == "TERMINATED" then target else "ALIVE")) s else s
      forM_ s' $ \(player, target) ->
        putText (player <> " -> " <> target)
  where
    opts = info (cmdLine <**> helper)
      ( fullDesc
     <> progDesc "Manage a game of assassin"
     <> header "assassin -- a command line utility to manage a game of assassin" )
