import Data.Time
import System.Cmd
import System.IO
import Control.Monad

bench_prefix = "\
\import Paper\n\
\\n\
\main = go (99999::Int) where\n\
\--     go i = if i == 0 then return() else go (i - (hSum (make i)))\n\
\--     go i = if i == 0 then return() else go (i - (hSum (hUpdate l2 (l2 .=. (1::Int)) (make i))))\n\
\    go i = if i == 0 then return() else go (i - (make i # l2))\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i = list\n\
\\n\
\list =\n"

--ghcs = ["ghc-7.4.1", "ghc-7.5.20120526"]
ghcs = ["ghc-7.5.20120526"]
--iters = [0, 50..1000]
iters = [950]
--records = ["Record", "SkewRecord", "ArrayRecord"]
records = ["ArrayRecord"]
dir = "bench/"
call ghc file = system $ ghc ++ " --make " ++ file ++ " -fcontext-stack=999 " ++
                " -odir " ++ dir ++ ghc ++ "_dir " ++
                " -hidir " ++ dir ++ ghc ++ "_dir " ++ 
                " >&2"

main = do
  system "make paper.hs"
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir
  
  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "paper.hs"
    
    -- headers
  putStr "iterations"
  forM_ ghcs $ \ghc -> forM_ records $ \record ->
      putStr $ ", " ++ ghc ++ " " ++ record
  putChar '\n'
  
  -- body
  forM_ iters $ \iter -> do
    putStr $ show iter ++ ", "
    forM_  ghcs $ \ghc -> forM_ records $ \record -> do
      let run = dir ++ "run_" ++ show iter ++ "_" ++ ghc ++ "_" ++ record ++ ".hs"
      let l1 = "    l1 .=. (0::Int) .*.\n"
      let l2 = "    l2 .=. (1::Int) .*.\n"
      writeFile run $ bench_prefix ++ concat (replicate iter l1) ++ l2 ++ "    empty" ++ record
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038
      start <- getCurrentTime
      call ghc run
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
      hFlush stdout
    putChar '\n'