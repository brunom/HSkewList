import Data.Time
import System.Cmd
import System.IO
import Control.Monad

compile_prefix = "\
\import Paper\n\
\\n\
\main = go (99999::Int) where\n\
\--     go i = if i == 0 then return() else go (i - (hSum (make i)))\n\
\--     go i = if i == 0 then return() else go (i - (hSum (hUpdate l2 (l2 .=. (1::Int)) (make i))))\n\
\    go i = if i == 0 then return() else go (i - (make i # L2))\n\
\\n\
\{-# NOINLINE make #-}\n\
\make i = list\n\
\\n\
\list =\n"

insert_prefix = "\
\import Paper\n\
\\n\
\main = go nil (iters::Int) where\n\
\    go r i = if i == 0 then return() else let r' = (L1 .=. 1 `cons` r) in go r' (i - (r' # L1))\n\
\ "

main = bench_insert
--ghcs = ["ghc-7.4.2", "ghc-7.6.1"]
ghcs = ["ghc-7.6.1"]
iters = [0, 50..0]
data Record = Record { name :: String, cons :: String, nil :: String, get :: String }
records = [Record "Record" "`HCons`" "HNil" "hListGet"
--          ,Record "SkewRecord" "`hSkewExtend`" "HNil" "\\r f -> case hSkewGet r f of HJust b -> b"
--          ,Record "ArrayRecord" "`hArrayExtend`" "arrayEmptyRecord" "hArrayGet"           
          ] 
dir = "bench/"

call ghc file = system $ ghc ++ " --make " ++ file ++ " -fcontext-stack=9999 " ++
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
      let l1 = "    (L1 .=. (0::Int)) " ++ cons record
      let l2 = "    (L2 .=. (1::Int)) " ++ cons record
      writeFile run $ compile_prefix ++ concat (replicate iter l1) ++ l2 ++ "  " ++ nil record ++ "\n(#) = " ++ get record
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038
      start <- getCurrentTime
      call ghc run
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
      hFlush stdout
    putChar '\n'


bench_insert = do
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
      let run = dir ++ "insert_" ++ show iter ++ "_" ++ ghc ++ "_" ++ name record ++ ".hs"
      writeFile run $ insert_prefix ++ "\niters = " ++ show iter ++ "\ncons a b = a " ++ cons record ++ "b \nnil = " ++ nil record ++ "\n(#) = " ++ get record
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038
      call ghc run
      start <- getCurrentTime
      system $ dir ++ ghc ++ "_dir/Main"
      end   <- getCurrentTime
      putStr $ show (end `diffUTCTime` start) ++ ", "
      hFlush stdout
    putChar '\n'
