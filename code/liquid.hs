{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import LIO
import LIO.LIORef
import LIO.Concurrent
import qualified Data.Set as Set
import Data.Set (Set)


import Control.Concurrent.MVar
import Control.Monad.State
import LIO.DCLabel
import LIO.TCB (ioTCB)

-- Implementation with dynamic checks

data FSRef l a = FSRefTCB Int (LIORef l (Labeled l a))

type FSState = (MVar Int, Set Int)

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
           lx <- label l x
           lcur <- getLabel
           newLIORef lcur lx
  (mv, addrs) <- getTCB 
  n <- liftLIO . ioTCB $ modifyMVarMasked mv $ \n -> return (n+1, n)
  putTCB (mv, Set.insert n addrs)
  return $ FSRefTCB n ref

readFSRef :: Label l => FSRef l a -> FSLIO l a
readFSRef (FSRefTCB a ref) = do
  (_, addrs) <- getTCB
  unless (Set.member a addrs) $ fail "Reference out of scope"
  liftLIO $ do
    taint $ labelOf ref      -- lcur = lcur \join l1 
    lv <- readLIORef ref     -- lcur = lcur \join l1 ?
    unlabel lv               -- lcur = lcur \join l1 \join l2 

writeFSRef :: Label l => FSRef l a -> a -> FSLIO l ()
writeFSRef (FSRefTCB a ref) x = do
  (_, addrs) <- getTCB
  unless (Set.member a addrs) $ fail "Reference out of scope"
  liftLIO $ do
    forkLIO $ do
      lv <- readLIORef ref            -- lcur' = lcur \join l1 
      lv' <- label (labelOf lv) x     -- label l1 x => lcur' <= l2, it means 
                                      -- lcur \join l1 <= l2, then 
                                      -- lcur <= lcur \join l1 <= l2 \join l1 
                                      -- lcur <= l1 \join l2 (as before) 
      writeLIORef ref lv'             -- lcur <= l1 

evalFSLIO :: FSLIO l a -> LIOState l -> IO a
evalFSLIO (FSLIOTCB act) s = do
  mv <- newMVar 0
  evalLIO (evalStateT act (mv, Set.empty)) s


evalFSDC :: FSLIO DCLabel a -> IO a
evalFSDC act = evalFSLIO act dcDefaultState


main = evalFSDC $ do
  ref <- newFSRef ("yo" %% True) "w00t"
  x <- readFSRef ref
  writeFSRef ref $ "yo " ++ x
  readFSRef ref

-- now I remember why I wrote lcur [= l `lub` l' in writeRef-FS:
-- the lcur [= l and  lcur [= l' is too inflexible: doesn't satisfy my
-- "good reference API" criterion
