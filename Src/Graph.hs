{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Graph
    ( GraphSt
    , GraphIndex
    , graphResolve
    , graphInsertDep
    , withGraph
    , graphLoop
    ) where

import Control.Applicative
import qualified Data.Map as M
import Control.Monad.State

-- represent the element in the graph
type GraphIndex = Int

data GraphSt a = GraphSt
    { nextIndex  :: !GraphIndex
    , indexTable :: M.Map a GraphIndex
    , depsTable  :: M.Map GraphIndex [GraphIndex]
    } deriving (Show,Eq)

-- | return the graph index of a 'a' object.
-- if it doesn't exist, it create one
graphResolve :: (Monad m, Ord a) => a -> StateT (GraphSt a) m GraphIndex
graphResolve pn = get >>= addOrGet
    where addOrGet st = maybe (add st) return $ M.lookup pn (indexTable st)
          add st = put (st { nextIndex = ni+1, indexTable = M.insert pn ni (indexTable st) }) >> return ni
                    where ni = nextIndex st

graphIsProcessed :: (Functor f, Monad f) => GraphIndex -> StateT (GraphSt a) f Bool
graphIsProcessed pn = M.member pn <$> gets depsTable

graphInsertDep :: Monad m => GraphIndex -> GraphIndex -> StateT (GraphSt a) m ()
graphInsertDep i j = modifyDepsTable i appendOrCreateList
  where appendOrCreateList Nothing  = Just [j]
        appendOrCreateList (Just z) = Just (j:z)
        modifyDepsTable k f = modify (\st -> st { depsTable = M.alter f k (depsTable st) })

withGraph :: (Functor m, Monad m)
          => StateT (GraphSt a) m ()
          ->  m (M.Map a GraphIndex, M.Map GraphIndex [GraphIndex])
withGraph f = (\st -> (indexTable st, depsTable st))
          <$> execStateT f (GraphSt 1 M.empty M.empty)

graphLoop :: Ord a
          => (a -> IO [a])
          -> a
          -> StateT (GraphSt a) IO ()
graphLoop getEdges = loop (0 :: Int)
  where loop !depth pn
            | depth > 1000 = error "internal error: infinite loop detected"
            | otherwise    = do
                pni       <- graphResolve pn
                processed <- graphIsProcessed pni
                --liftIO $ putStrLn ("loop: " ++ show pn ++ " processed: " ++ show processed ++ " depth: " ++ show depth)
                unless processed $ do
                    depNames <- filter (/= pn) <$> liftIO (getEdges pn)
                    mapM_ (loop (depth+1)) depNames
                    mapM_ (graphResolve >=> graphInsertDep pni) depNames
