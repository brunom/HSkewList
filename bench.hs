import Data.Time
import System.Process
import System.IO
import Control.Monad

main = bench_get
ghcs = ["ghc-7.6.1", "ghc-8.2.2", "ghc-8.4.1"]
--ghcs = ["ghc"]
--iters = [0]
iters = [200]
--iters = [0, 25..200]

data Record = Record { name :: String, cons :: String, nil :: String, get :: String, update :: String }
records =
    --Record "Record" "HCons" "HNil" "hListGet" "hListUpdate" :
    Record "ArrayRecord" "hArrayExtend" "emptyArrayRecord" "hArrayGet" "hArrayUpdate" :
    --Record "SkewRecord" "hSkewExtend" "emptySkewRecord" "\\r f -> case hSkewGet r f of HJust b -> b" "hSkewUpdate" :
    []
dir = "bench/"

call ghc file = system $ ghc ++ " --make " ++ file ++ " -O -fcontext-stack=9999 " ++
                      " -odir " ++ dir ++ ghc ++ "_dir " ++
                      " -hidir " ++ dir ++ ghc ++ "_dir " ++ 
                      " >&2"

bench_get = do
  system "make Paper.hs >&2"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  

  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "Paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record -> do
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " compile"
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " run"
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    hFlush stdout
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}\n\
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
      hFlush stdout

      startRun <- getCurrentTime
      --system $ dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record
      endRun   <- getCurrentTime
      putStr $ show (endRun `diffUTCTime` startRun) ++ ", "
      hFlush stdout
    putChar '\n'

bench_update = do
  system "make Paper.hs >&2"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  

  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "Paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record -> do
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " compile"
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " run"
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    hFlush stdout
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (9999999::Int) where\n\
\    go i = if i == 0 then return() else go (i - get (update L2 (L2 .=. 1) (make i)) L2)\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i = list\n\
\\n\
\list =\n" ++
        concat (replicate iter " cons (L1 .=. (0::Int)) $\n") ++
        " cons (L2 .=. \"foo\") nil" ++
        "\ncons = " ++ cons record ++
        "\nnil = " ++ nil record ++
        "\nget = " ++ get record ++
        "\nupdate = " ++ update record ++ "\n"
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038

      startCompile <- getCurrentTime
      call ghc run
      endCompile <- getCurrentTime
      putStr $ show (endCompile `diffUTCTime` startCompile) ++ ", "
      hFlush stdout

      startRun <- getCurrentTime
      system $ dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record
      endRun   <- getCurrentTime
      putStr $ show (endRun `diffUTCTime` startRun) ++ ", "
      hFlush stdout
    putChar '\n'

bench_extend = do
  system "make Paper.hs >&2"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  
  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "Paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record -> do
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " compile"
      putStr $ ", " ++ ghc ++ " " ++ name record ++ " run"
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    hFlush stdout
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "extend_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\ 
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (999999::Int) where\n\
\    go i = if i == 0 then return() else go (i - (get (make i) L1) + i - 1)\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i = L1 .=. i `cons` list\n\
\\n\
\list =\n" ++
        concat (replicate iter " cons (L2 .=. (0::Int)) $\n") ++
        "  nil" ++
        "\ncons = " ++ cons record ++
        "\nnil = " ++ nil record ++
        "\nget = " ++ get record ++ "\n"
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038
      call ghc run

      startCompile <- getCurrentTime
      call ghc run
      endCompile <- getCurrentTime
      putStr $ show (endCompile `diffUTCTime` startCompile) ++ ", "
      hFlush stdout

      startRun <- getCurrentTime
      system $ dir ++ "extend_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record
      endRun   <- getCurrentTime
      putStr $ show (endRun `diffUTCTime` startRun) ++ ", "
      hFlush stdout
    putChar '\n'
