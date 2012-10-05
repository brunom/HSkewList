import Data.Time
import System.Cmd
import System.IO
import Control.Monad

main = bench_get
--ghcs = ["ghc-7.4.2", "ghc-7.6.1"]
ghcs = ["ghc-7.6.1"]
iters = [0, 100..500]
data Record = Record { name :: String, cons :: String, nil :: String, get :: String }
records = [Record "Record" "HCons" "HNil" "hListGet"
          ,Record "SkewRecord" "hSkewExtend" "HNil" "\\r f -> case hSkewGet r f of HJust b -> b"
          ,Record "ArrayRecord" "hArrayExtend" "arrayEmptyRecord" "hArrayGet"           
          ] 
dir = "bench/"

call ghc file = system $ ghc ++ " --make " ++ file ++ " -O -fcontext-stack=9999 " ++
                      " -odir " ++ dir ++ ghc ++ "_dir " ++
                      " -hidir " ++ dir ++ ghc ++ "_dir " ++ 
                      " >&2"

bench_compile = do
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
      let run = dir ++ "compile_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (99999::Int) where\n\
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
      start <- getCurrentTime
      call ghc run
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
      hFlush stdout
    putChar '\n'

bench_get = do
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
      let run = dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Paper\n\
\\n\
\main = go (99999::Int) where\n\
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
      call ghc run
      start <- getCurrentTime
      system $ dir ++ "get_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
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
