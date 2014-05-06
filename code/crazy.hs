import LIO
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Dynamic
import Control.Monad
import Control.Concurrent.MVar
import LIO.TCB
import System.IO.Unsafe


import LIO.DCLabel

-- Implementation with dynamic checks

gRefStore :: MVar (Map Int Dynamic)
{-# NOINLINE gRefStore #-}
gRefStore = unsafePerformIO $ newMVar Map.empty

data Ref l t = RefTCB l Int

newRef :: (Label l, Typeable t) => l -> t -> LIO l (Ref l t)
newRef l x = do
  guardAlloc l
  lcur <- getLabel
  n <- ioTCB $ modifyMVarMasked gRefStore $ \store ->
     let n = Map.size store
     in return (Map.insert n (toDyn (LabeledTCB l x)) store, n)
  return $ RefTCB lcur n


readRef :: (Label l, Typeable t) => Ref l t -> LIO l t
readRef (RefTCB l n) = do
  taint l
  lcur <- getLabel
  store <- ioTCB $ readMVar gRefStore
  when (Map.notMember n store) $ fail "BUG: invalid address"
  case fromDynamic $ store Map.! n of
    Just (LabeledTCB l' x) -> do
      taint l'
      return x
    _ -> fail "BUG: casting failed"


main = evalDC $ do
 r <- newRef dcPublic "w00t"
 readRef r
