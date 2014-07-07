module Main

import IQuery

push : StateC (STList STString) -> Event Click -> IO Int
push s e = do
  Just input <- query "input#pushVal" >>= (\x => elemAt x 0)
  Just xs    <- get s =>> fromState
  text <- getValue input
  get s :=> toState (text :: xs)
  pure 1

shift : StateC (STList STString) -> Event Click -> IO Int
shift s e = do
  Just x <- get s =>> access 0 =>> fromState
    | Nothing => do
                 alert "stack is empty" 
                 pure 1
  alert x
  Just (_::xs) <- get s =>> fromState
  get s :=> toState xs
  pure 1

instance Show Key where
  show KeySpace = "spc"
  show KeyEnter = "enter"
  show _ = "_"

foo : Event KeyDown -> IO Int
foo e = do
    k <- key e
    alert $ show k
    pure 1
   
main : IO ()
main = do
  queue <- newState (STList STString) Nil
  Just i <- !(query "input#pushVal") `elemAt` 0
  onEvent i foo
  Just p <- !(query "input#pushAct") `elemAt` 0
  onClick p (push queue) 
  Just s <- !(query "input#shiftAct") `elemAt` 0
  onClick s (shift queue) 
  pure ()
