import Data.Time
import System.Cmd
import Control.Monad

line = "    l1 .=. (0::Int) .*."
ghcs = ["ghc-7.4.1", "ghc-7.5.20120526"]
iters = [0, 50, 100, 150, 200]
dir = "bench/"
flags ghc = " -odir " ++ dir ++ ghc ++ "_dir -hidir " ++ dir ++ ghc ++ "_dir "  

time a = do
    start <- getCurrentTime
    v <- a
    end   <- getCurrentTime
    let diff = end `diffUTCTime` start
    putStr $ show diff ++ ", "
    return diff

main = do
  system "make paper.hs"
  system $ "mkdir " ++ dir
  file <- readFile "paper.hs"
  let all = lines file
  let prefix = takeWhile (/= line) all
  let line_suffix = dropWhile (/= line) all
  let suffix = dropWhile (== line) line_suffix
  forM_ ghcs $ \ghc -> do
    putStr "iterations, "
    let warmup = dir ++ "warmup_" ++ ghc ++ ".hs"
    writeFile warmup . unlines $ prefix ++ suffix
    system $ ghc ++ " --make" ++ flags ghc ++ warmup ++ " > /dev/null"
    putStr ghc
  putChar '\n'
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    forM_  ghcs $ \ghc -> do
      let run = dir ++ "run_" ++ show iter ++ "_" ++ ghc ++ ".hs"
      writeFile run . unlines $ prefix ++ replicate iter line ++ suffix
      time $ system $ ghc ++ " --make" ++ flags ghc ++ run ++ " > /dev/null"
    putChar '\n'