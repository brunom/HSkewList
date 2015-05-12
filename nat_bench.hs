import Data.Time
import System.Process
import System.IO
import Control.Monad

main = bench
--ghcs = ["ghc-7.4.2", "ghc-7.6.1"]
ghcs = ["ghc"]
data Nat = Nat { name :: String, z :: String, s :: String, nth :: String }
nats =
  Nat "skew" "(SSkew SNil)" "sSkewSucc" "skewNth" :
  --Nat "unary" "SZ" "SS" "unaryNth" :
  []
iters = [1000]


dir = "nat_bench/"

call ghc file =
  (system $ ghc ++ " --make " ++ file ++ " -O -fcontext-stack=9999 -ftype-function-depth=9999" ++
                      " -odir " ++ dir ++ ghc ++ "_dir " ++
                      " -hidir " ++ dir ++ ghc ++ "_dir " ++
                      " >&2")
           --            >>
  -- (system $ ghc ++ " -prof -fprof-auto -osuf p_o --make " ++ file ++ " -O -fcontext-stack=9999 -ftype-function-depth=9999" ++
  --                     " -odir " ++ dir ++ ghc ++ "_dir " ++
  --                     " -hidir " ++ dir ++ ghc ++ "_dir " ++
  --                     " >&2")

bench = do
  system $ "rm -rf " ++ dir
  system $ "mkdir " ++ dir


  -- precompile dependencies
  forM_ ghcs $ \ghc -> do
    call ghc "nat.hs"

  -- body
  forM_ iters $ \iter -> do
    --putStr $ show iter ++ ", "
    hFlush stdout
    forM_  ghcs $ \ghc -> forM_ nats $ \nat -> do
      let run = dir ++ name nat ++ "_" ++ show iter ++ "_" ++ ghc ++ ".hs"
      writeFile run $ "\
\{-# LANGUAGE NoMonomorphismRestriction #-}\n\
\import Nat\n\
\import Data.Singletons.TH\n\
\import Data.Singletons.Prelude\n\
\import Data.Singletons.Prelude.Base\n\
\import GHC.Int\n\
\\n\
\main = return $ go2 (-1::Int64) where\n\
\    go i = if i == 0 then 1 else go (i - " ++ nth nat ++" (make_list i) (make_index i))\n\
\    go2 i = if i == 0 then 1 else go2 (i - go (-1::Int64))\n\
\\n\
\{-# NOINLINE make_list #-}\n\
\make_list _ = list\n\
\\n\
\list =\n" ++
        concat (replicate iter " VecCons 0 $\n") ++
        " VecCons 1 VecNil\n" ++ "\
\\n\
\{-# NOINLINE make_index #-}\n\
\make_index _ = index\n\
\\n\
\index =\n" ++
        concat (replicate iter (" " ++ s nat ++ " $\n")) ++
        " " ++ z nat ++ "\n"
      system $ "rm " ++ dir ++ ghc ++ "_dir/Main.*" -- workaround http://hackage.haskell.org/trac/ghc/ticket/7038

      putStr $ name nat ++ ", "
      startCompile <- getCurrentTime
      call ghc run
      endCompile <- getCurrentTime
      putStr $ show (endCompile `diffUTCTime` startCompile) ++ ", "
      hFlush stdout

      startRun <- getCurrentTime
      system $ dir ++ name nat ++ "_" ++ show iter ++ "_" ++ ghc
      endRun   <- getCurrentTime
      putStr $ show (endRun `diffUTCTime` startRun) ++ ", "
      putChar '\n'
      hFlush stdout
