import Data.Time
import System.Cmd
import System.IO
import Control.Monad

main = bench_get
--ghcs = ["ghc-7.4.2", "ghc-7.6.1"]
ghcs = ["ghc-7.6.1"]
--iters = [0,10,20,30,40,50,60,70,100,150,200]
iters = [0, 25..400]

data Record = Record { name :: String, cons :: String, nil :: String, get :: String }
records = [Record "Record" "HCons" "HNil" "hListGet"
          ,Record "ArrayRecord" "hArrayExtend" "emptyArrayRecord" "hArrayGet"           
          ,Record "SkewRecord" "hSkewExtend" "emptySkewRecord" "\\r f -> case hSkewGet r f of HJust b -> b"
          ] 
dir = "bench/"

call ghc file = system $ ghc ++ " --make " ++ file ++ " -O -fcontext-stack=9999 " ++
                      " -odir " ++ dir ++ ghc ++ "_dir " ++
                      " -hidir " ++ dir ++ ghc ++ "_dir " ++ 
                      " >&2"

bench_get = do
  system "make paper.hs >&2"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  

  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record -> do
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " compile"
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " run"
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (9999999::Int) where\n\
\    go i = if i == 0 then return() else go (i - get (make i) L2)\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i = list\n\
\\n\
\list =\n" ++
        concat (replicate iter " cons (L1 .=. (0::Int)) $\n") ++
        " cons (L2 .=. (1::Int)) nil" ++
        "\ncons = " ++ cons record ++
        "\nnil = " ++ nil record ++
        "\nget = " ++ get record ++ "\n"
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038

      startCompile <- getCurrentTime
      call ghc run
      endCompile <- getCurrentTime
      putStr $ show (endCompile `diffUTCTime` startCompile) ++ ", "

      startRun <- getCurrentTime
      --system $ dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record
      endRun   <- getCurrentTime

      putStr $ show (endRun `diffUTCTime` startRun) ++ ", "
      hFlush stdout
    putChar '\n'

bench_extend = do
  system "make paper.hs >&2"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  
  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record ->
      putStr $ ", " ++ ghc ++ " " ++ name record
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "extend_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\ 
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (99999::Int) where\n\
\    go i = if i == 0 then return() else go (i - (get (make i) L1) + i - 1)\n\
\\n\
\step r = (L1 .=. (get r L1)) `cons` r\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i =\n" ++
        concat (replicate iter " step $\n") ++
        "  (L1 .=. i `cons` nil)" ++
        "\ncons = " ++ cons record ++
        "\nnil = " ++ nil record ++
        "\nget = " ++ get record ++ "\n"
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038
      call ghc run
      start <- getCurrentTime
      system $ dir ++ ghc ++ "_dir/Main"
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
      hFlush stdout
    putChar '\n'
