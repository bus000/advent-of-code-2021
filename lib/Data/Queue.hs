module Data.Queue
    -- Type.
    ( Queue

    -- Construction.
    , empty
    , singleton
    , fromList

    -- Operations.
    , enqueue
    , dequeue

    -- Destruction.
    , toList
    ) where

data Queue a = Queue
    { _push :: ![a]
    , _pull :: ![a]
    } deriving (Show, Eq, Ord)

instance Functor Queue where
    fmap f (Queue push pull) = Queue (map f push) (map f pull)

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton x = Queue [] [x]

fromList :: [a] -> Queue a
fromList xs = Queue [] xs

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue push pull) = Queue (x:push) pull

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue (Queue [] []) = (empty, Nothing)
dequeue (Queue push (x:pull)) = (Queue push pull, Just x)
dequeue (Queue push []) = dequeue $ Queue [] (reverse push)

toList :: Queue a -> [a]
toList (Queue push pull) = pull ++ reverse push
