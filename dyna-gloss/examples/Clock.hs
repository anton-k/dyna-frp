import Dyna

constTick :: Frp m => NominalDiffTime -> a -> Evt m a
constTick t v = gen t v id

ones, nats :: Frp m => Evt m Int
ones = constTick 1 1
nats = sumE ones


lines :: Frp m => Evt m String
lines = Evt $ \proc -> proc =<< liftIO getLine

gen :: forall m s . (Frp m) => NominalDiffTime -> s -> (s -> s) -> Evt m s
gen t seed f = res
  where
    res = Evt $ \go -> do
      tv <- proxyNewRef res seed
      periodic t $ do
        go =<< liftIO (readRef tv)
        liftIO $ modifyRef tv f

    -- debug for clearing info
    -- putStrLn "boo!"

countSeconds :: Frp m => Evt m Int
countSeconds = gen 1 0 succ
