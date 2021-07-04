module Negation where
import Enviroment
import LSyntax




-------------------------------------------------------------
-- Helper Functions to build Stack
-------------------------------------------------------------


mainInstructCycle :: Program -> Bool
mainInstructCycle prg =
    mainInstructCycle' env (Stack $ Map.fromList []) (B False) (C Nothing) (P (goalC env)) (R Nothing) (T (-1))
    where
        env = translate prg
        mainInstructCycle' :: Env -> Stack 
                              -> RegisterB -> RegisterC -> RegisterP -> RegisterR -> RegisterT
                              -> Bool
        mainInstructCycle' env stack b c p r t = case p of
            Unify atom     -> mainInstructCycle' env stack (B (unify atom stack)) c (p+1) r t
            Push atom      -> mainInstructCycle' env (push atom c p stack env) b (stackSize stack + 1) (p+1) (R (c+1)) (stackSize stack + 4)
            PushNot neg    -> mainInstructCycle' env (pushNot neg c p stack env) b (stackSize stack+1) (p+1) (R (c+1)) (stackSize stack + 4)
            Backtrack      -> mainInstructCycle' env stack (B (backtrack)) 
            Call           -> mainInstructCycle' env stack (B (call b c p stack)) c (P (call b c p stack)) r t 
            ReturnPos      -> mainInstructCycle' env stack b c (P stack.R) (R returnpos r stack) t
          --ReturnNeg      -> mainInstructCycle' env (stack !! r-1 <- nil) False c (P let p = returnneg stack r p of p -> (p,r)) 
                                                                                --(R let r = returnneg stack r p of r -> (p,r)) t
	        ReturnNeg      -> mainInstructCycle' env stack r p
            Prompt         -> mainInstructCycle' env stack (B (prompt b p stack))

-------------------------------------------------------------
-- Functions for InstructionCycle
-------------------------------------------------------------
pushNot :: Neg -> RegisterC -> RegisterP -> Stack -> Env -> Stack
pushNot not c p (Stack stack) env = let n = stackSize (Stack stack) in
    Map.insert (n+1) (nil)        (Map.fromList(Stack stack))
    Map.insert (n+2) (c)          (Map.fromList(Stack stack))
    Map.insert (n+3) (p+4)        (Map.fromList(Stack stack))
    Map.insert (n+4) (not)        (Map.fromList(Stack stack))

push :: Atom -> RegisterC -> RegisterP -> Stack -> Env -> Stack
push atom c p (Stack stack) env = let n = stackSize (Stack stack) in
    Map.insert (n+1) (firstC env) (Map.fromList(Stack stack))
    Map.insert (n+2) (c)          (Map.fromList(Stack stack))
    Map.insert (n+3) (p+3)        (Map.fromList(Stack stack))
    Map.insert (n+4) (atom)       (Map.fromList(Stack stack))

unify :: Atom -> RegisterB -> RegisterT -> Stack -> Stack
unify atom b t (Stack stack) = case t of 
    atom -> (B True)
    _ -> nothing

returnneg :: Stack -> RegisterR -> RegisterP -> (RegisterP, RegisterR)
returnneg = undefined
--returnneg stack r p = case Stack !! r of 
--   nil -> (p+1,r)
--    _ -> ((stack !! r + 1), (Stack !! r) + 1 )   


returnpos :: RegisterR -> Stack -> RegisterR
returnpos = undefined
--returnpos r (Stack stack) = let n = stackSize (Stack stack) in
--    ((stack !! r) /= nil) -> (stack !! r) + 1

call :: RegisterB -> RegisterC -> RegisterP -> Env -> Stack ->  Stack
call b c p (Stack stack) env = case c of 
    nil -> (B True) (p+1)
    _ -> (P (nextC env)) 

backtrack :: RegisterB -> RegisterC -> RegisterR -> RegisterT -> Stack -> Stack
backtrack p (Stack stack) = case p of-}

prompt :: RegisterB -> RegisterP ->  Stack -> Stack
prompt b p (Stack stack) = case b of 
    True -> putStrLn "no (more) solutions"
    False -> prompt' where

        
        ';' -> (B True) (p-1)
        _ -> (P (nil))


--mainInstructCycle :: Program -> Bool
--mainInstructCycle prg = undefined
-- !Data type of c Instructions can be changed by need
-- TODO: Define mainInstructionCycle 
-- TODO: Init Stacks Init Registers and
-- TODO: Bind syntax tree codes and this file
-- TODO: Define main Function with IO
