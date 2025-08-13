-- MatrixMonad.hs

-- A single-file Haskell program modeling Oracle/Architect as a contextual "monad"
-- which defines an Overton window of allowed Actions. Neo attempts actions;
-- only those in the Overton window succeed. Neo can question the monad to
-- expand possibilities, or exit to a freer topos.

{-# LANGUAGE DeriveGeneric #-}

import Control.Monad.State
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import GHC.Generics (Generic)

-- Domain
data Action
  = RedPill
  | BluePill
  | Door Int
  | BothPills
  | Reinvent
  | QuestionMonad -- attempt to question the context
  | ExitMonad    -- move to a different topos (escape/compose outside)
  deriving (Eq, Ord, Show, Generic)

type Overton = [Action]

data Neo = Neo
  { nName   :: String
  , aware   :: Bool
  , history :: [Action]
  } deriving (Show)

data World = World
  { overton :: Overton -- current allowed actions (the monad's catalogue)
  , neo     :: Neo
  } deriving (Show)

-- Initialize a constrained world: only pills and a couple of doors
initWorld :: World
initWorld = World
  { overton = [RedPill, BluePill, Door 1, Door 2]
  , neo     = Neo { nName = "Neo", aware = False, history = [] }
  }

-- Utility: pretty-print the Overton window
showOverton :: Overton -> String
showOverton os = intercalate ", " (map show os)

-- Attempt an action: allowed only if action ∈ overton, else it's an illusion
attempt :: Action -> StateT World IO ()
attempt act = do
  w <- get
  let os = overton w
      n  = neo w
  liftIO $ putStrLn $ "\n>> Attempting: " ++ show act
  if act `elem` os
    then do
      -- action succeeds: update Neo's history and possibly mutate the Overton window
      let n' = n { history = history n ++ [act] }
      put $ w { neo = n' }
      liftIO $ putStrLn $ "✔ Action permitted by the monad. (Recorded in history.)"
      applyConsequences act
    else do
      liftIO $ putStrLn $ "✖ That choice is an illusion — not inside the Overton window."
      liftIO $ putStrLn $ "Hint: try 'question' to widen the frame or 'oracle'/'architect' to interact."

-- Consequences: simple rules for how actions can change the world
applyConsequences :: Action -> StateT World IO ()
applyConsequences act = case act of
  RedPill -> liftIO $ putStrLn "You wake. The rules feel slightly different, but still framed."
  BluePill -> liftIO $ putStrLn "You remain. Patterns persist; the catalog unchanged."
  Door n -> liftIO $ putStrLn $ "You step through Door " ++ show n ++ ". The room is engineered."
  BothPills -> do
    liftIO $ putStrLn "You take both. The monad stutters; coherence bangs like a struck gong."
    -- Taking both forces the architect to allow a new small possibility:
    modify $ \w -> w { overton = uniqueAppend (overton w) [Reinvent] }
  Reinvent -> do
    liftIO $ putStrLn "You reinvent the map: choices recompose. New morphisms become available."
    -- Reinvention broadens the Overton window drastically
    modify $ \w -> w { overton = uniqueAppend (overton w) [BothPills, Reinvent, ExitMonad] }
  QuestionMonad -> do
    liftIO $ putStrLn "You ask 'why these and not others?' Awareness increases."
    modify $ \w -> w { neo = (neo w) { aware = True }, overton = uniqueAppend (overton w) [BothPills] }
  ExitMonad -> do
    liftIO $ putStrLn "Neo moves to another topos — rules changed. The Architect's binder loses grip."
    -- New topos: essentially full freedom (for demo, we add many actions)
    modify $ \w -> w { overton = [RedPill, BluePill, Door 1, Door 2, BothPills, Reinvent, ExitMonad] }

-- Helper: append unique items to the overton window
uniqueAppend :: Overton -> Overton -> Overton
uniqueAppend base extra = base ++ filter (`notElem` base) extra

-- Oracle and Architect: they can be invoked to nudge or reassert the monad.
oracle :: StateT World IO ()
oracle = do
  liftIO $ putStrLn "\n[Oracle] I will hint at a path, but not hand you the map."
  modify $ \w -> w { overton = uniqueAppend (overton w) [Door 3] }

architect :: StateT World IO ()
architect = do
  liftIO $ putStrLn "\n[Architect] I will prune or re-assert structure."
  -- Simulate system tightening: remove Reinvent if present; keep pills and door1/2
  modify $ \w -> w { overton = filter (`elem` [RedPill, BluePill, Door 1, Door 2]) (overton w) }

-- Status print
status :: StateT World IO ()
status = do
  w <- get
  let n = neo w
  liftIO $ putStrLn $ "\n--- STATUS ---"
  liftIO $ putStrLn $ "Neo: " ++ nName n ++ " | aware: " ++ show (aware n)
  liftIO $ putStrLn $ "History: " ++ show (history n)
  liftIO $ putStrLn $ "Overton window: " ++ showOverton (overton w)
  liftIO $ putStrLn $ "---------------"

-- Interactive loop: parse simple commands
repl :: StateT World IO ()
repl = do
  liftIO $ putStr "\ncommand> "
  liftIO $ hFlush stdout
  line <- liftIO getLine
  case words (map toLowerSafe line) of
    [] -> repl
    ("help":_) -> do
      liftIO $ putStrLn helpText
      repl
    ("status":_) -> status >> repl
    ("oracle":_) -> oracle >> repl
    ("architect":_) -> architect >> repl
    ("question":_) -> attempt QuestionMonad >> repl
    ("red":_) -> attempt RedPill >> repl
    ("blue":_) -> attempt BluePill >> repl
    ("both":_) -> attempt BothPills >> repl
    ("reinvent":_) -> attempt Reinvent >> repl
    ("door":num:_) -> case reads num of
      [(n,"")] -> attempt (Door n) >> repl
      _ -> liftIO (putStrLn "door <n> where n is a number") >> repl
    ("exit":_) -> attempt ExitMonad >> repl
    ("quit":_) -> liftIO $ putStrLn "Goodbye."
    _ -> do
      liftIO $ putStrLn "Unknown command. Type 'help' for options."
      repl

toLowerSafe :: Char -> Char
toLowerSafe c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

helpText :: String
helpText = unlines
  [ "commands:"
  , " help -- show this text"
  , " status -- show Neo and Overton window"
  , " oracle -- Oracle hints (adds a door)"
  , " architect -- Architect tightens the system"
  , " question -- Neo questions the monad (may add BothPills)"
  , " red -- attempt RedPill"
  , " blue -- attempt BluePill"
  , " both -- attempt to take both pills"
  , " door <n> -- attempt Door n (e.g., door 1)"
  , " reinvent -- attempt to reinvent choices (if available)"
  , " exit -- attempt to exit to another topos"
  , " quit -- leave program"
  ]

-- Entry
main :: IO ()
main = do
  putStrLn "=== Matrix Monad interactive demo ===\n"
  putStrLn "You are Neo. The Oracle and Architect shape an Overton window of allowed choices."
  putStrLn "Type 'help' for commands."
  evalStateT repl initWorld
