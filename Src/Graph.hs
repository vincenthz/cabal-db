{-# LANGUAGE BangPatterns #-}
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

graphResolve pn = get >>= addOrGet
    where addOrGet st = maybe (add st) return $ M.lookup pn (indexTable st)
          add st = put (st { nextIndex = ni+1, indexTable = M.insert pn ni (indexTable st) }) >> return ni
                    where ni = nextIndex st

graphIsProcessed pn = M.member pn <$> gets depsTable

modifyDepsTable k f = modify (\st -> st { depsTable = M.alter f k (depsTable st) })
graphInsertDep i j = modifyDepsTable i f
    where f Nothing  = Just [j]
          f (Just z) = Just (j:z)

withGraph f = (\st -> (indexTable st, depsTable st)) <$> execStateT f (GraphSt 1 M.empty M.empty)

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
                    depNames <- liftIO $ getEdges pn
                    mapM_ (loop (depth+1)) depNames
                    mapM_ (graphResolve >=> graphInsertDep pni) depNames
