{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import LIO
import LIO.LIORef
import qualified Data.Set as Set
import Data.Set (Set)

import Data.IORef


import Control.Monad.State
import LIO.DCLabel
import LIO.TCB.LObj (LObj(..))
import LIO.TCB

-- Implementation with dynamic checks

data FSRef l a = FSRefTCB Int (LIORef l (LIORef l a))

type FSState = (Int, Set Int)

newtype FSLIO l a = FSLIOTCB (StateT FSState (LIO l) a)
                  deriving (Monad)

instance Label l => MonadLIO l (FSLIO l) where
  liftLIO = FSLIOTCB . lift

getTCB :: FSLIO l FSState
getTCB = FSLIOTCB get

putTCB :: FSState -> FSLIO l ()
putTCB s = FSLIOTCB $ put s

newFSRef :: Label l => l -> a -> FSLIO l (FSRef l a)
newFSRef l x = do
  ref <- liftLIO $ do
           lx <- newLIORef l x
           lcur <- getLabel
           newLIORef lcur lx
  (n, addrs) <- getTCB 
  putTCB (n+1, Set.insert n addrs)
  return $ FSRefTCB n ref

readFSRef :: Label l => FSRef l a -> FSLIO l a
readFSRef (FSRefTCB a ref) = do
  (_, addrs) <- getTCB
  unless (Set.member a addrs) $ fail "Reference out of scope"
  liftLIO $ do
    lv <- readLIORef ref
    readLIORef lv

writeFSRef :: Label l => FSRef l a -> a -> FSLIO l ()
writeFSRef (FSRefTCB a ref) x = do
  (_, addrs) <- getTCB
  unless (Set.member a addrs) $ fail "Reference out of scope"
  liftLIO $ do
    discard $ do
      lv <- readLIORef ref
      writeLIORef lv x

upgradeFSRef :: Label l => FSRef l a -> l -> FSLIO l ()
upgradeFSRef (FSRefTCB a ref) l = do
  (_, addrs) <- getTCB
  unless (Set.member a addrs) $ fail "Reference out of scope"
  liftLIO $ do
    discard $ do
      lv  <- readLIORef ref
      lv' <- cloneLIORef lv l
      writeLIORef ref lv'

cloneLIORef :: Label l => LIORef l a -> l -> LIO l (LIORef l a)
cloneLIORef (LObjTCB l ref) l' = do
  lcur <- getLabel
  let lnew = lcur `lub` l `lub` l'
  v <- ioTCB $ readIORef ref
  newLIORef lnew  v


evalFSLIO :: (LIO l a -> LIOState l -> IO b) -> FSLIO l a -> LIOState l -> IO b
evalFSLIO eval (FSLIOTCB act) s = do
  eval (evalStateT act (0, Set.empty)) s


evalFSDC :: FSLIO DCLabel a -> IO a
evalFSDC act = evalFSLIO evalLIO act dcDefaultState

runFSDC :: FSLIO DCLabel a -> IO (a, LIOState DCLabel)
runFSDC act = evalFSLIO runLIO act dcDefaultState

main = forM_ [1..2] $ \_ -> do
 res <- runFSDC $ do
   ref <- newFSRef (True %% True) "w00t"
   x <- readFSRef ref
   writeFSRef ref $ "yo " ++ x
   upgradeFSRef ref ("yo" %% True)
   readFSRef ref
 print res

-- now I remember why I wrote lcur [= l `lub` l' in writeRef-FS:
-- the lcur [= l and  lcur [= l' is too inflexible: doesn't satisfy my
-- "good reference API" criterion






--
-- toLabeled
--

discard :: Label l => LIO l () -> LIO l ()
discard lio = do
 s <- getLIOStateTCB
 ioTCB $ void $ do
    ((), _) <- runLIO lio s
    return ()
 putLIOStateTCB s
