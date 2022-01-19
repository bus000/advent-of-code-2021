{-# LANGUAGE LambdaCase #-}
module Control.Monad.Misc
    ( firstJustM
    ) where

-- Call monadic function on each element of the list stopping on the first Just
-- value.
firstJustM :: Monad m => (a -> m (Maybe b))  -> [a] -> m (Maybe b)
firstJustM _ [] = return Nothing
firstJustM f (x:xs) = f x >>= \case
    Just y -> return $ Just y
    Nothing -> firstJustM f xs
