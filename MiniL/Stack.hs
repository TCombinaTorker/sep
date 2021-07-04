import Enviroment
import LSyntax


-------------------------------------------------------------
-- Data Type Registers
-------------------------------------------------------------

data Stack = Stack (Map.Map Int StackCell) deriving (Show, Eq)

data StackCell = EnvAdress (Maybe Int) | AtomCell Atom | StackAdress (Maybe Int)
    deriving (Eq, Show)

stackSize :: Stack -> Int
stackSize (Stack stack) = Map.size stack

data RegisterB = B Bool

-- return register 
data RegisterR = R StackCell

-- top-of-stack register, usually: T (size stack) - 1 
data RegisterT = T StackCell

-- last choice point register 
data RegisterC = C StackCell

-- programcounter 
data RegisterP = P StackCell

-------------------------------------------------------------
-- Helperfunctions for InstructionCycle
-------------------------------------------------------------

rtoEnvCode :: RegisterR -> Env -> EnvCode
rtoEnvCode (R (EnvAdress (Just r))) (Env env) = Map.lookup r env


plusP :: Int -> StackCell -> StackCell
plusP _ (EnvAdress Nothing) = EnvAdress Nothing
plusP x (EnvAdress (Just p)) = (EnvAdress $ Just (x + p))

{-mainInstructCycle :: Program -> Bool
mainInstructCycle prg = 
    mainInstructCycle' env (Stack $ Map.fromList []) (B False) (C Nothing) (P (goalC env)) (R Nothing) (T (-1))
    where
        env = translate prg
        mainInstructCycle' :: Env -> Stack 
                              -> RegisterB -> RegisterC -> RegisterP -> RegisterR -> RegisterT
                              -> Bool
        mainInstructCycle' env stack b c p r t = case p of
            Unify atom -> mainInstructCycle' env stack (B (unify atom stack)) c (p+1) r t
            Push atom  -> mainInstructCycle' env (push atom c p stack env) b (stackSize stack + 1) (p+1) (R (c+1)) (stackSize stack + 4) 
            Backtrack  -> mainInstructCycle' env stack (B (backtrack)) 
            Call       -> mainInstructCycle' env stack (B (call b c p stack)) c (P (call b c p stack)) r t 
            Return     -> mainInstructCycle' env stack (B (return b c stack)) c (r+1) r t
            Prompt     -> mainInstructCycle' env stack (B (prompt b p stack))-}


-------------------------------------------------------------
-- Functions for InstructionCycle
-------------------------------------------------------------

push :: Atom -> RegisterC -> RegisterP -> Stack -> Env -> Stack
push atom c p stack env = Stack $ f4 (f3 (f2 (f1 stack n env) n c) n p) n atom where
    n = stackSize stack
    f1 :: Stack -> Int -> Env -> Map.Map Int StackCell
    f1 (Stack stack) n env = Map.insert (n+1) (EnvAdress $ Just $ fst (firstC env)) stack
    f2 :: Map.Map Int StackCell -> Int -> RegisterC -> Map.Map Int StackCell
    f2 map n (C c)         = Map.insert (n+2) c map
    f3 :: Map.Map Int StackCell -> Int -> RegisterP -> Map.Map Int StackCell 
    f3 map n (P p)         = Map.insert (n+3) (3 `plusP` p) map
    f4 :: Map.Map Int StackCell -> Int -> Atom -> Map.Map Int StackCell
    f4 map n atom  = Map.insert (n+4) (AtomCell atom) map

unify :: Atom -> RegisterB -> RegisterT -> Stack -> RegisterB
unify atom b t (Stack stack) = case t of 
    atom -> (B True)
    _    -> (B False)

return :: RegisterP -> RegisterR -> Stack -> RegisterR
return p r (Stack stack) = return' p r (Stack stack) where
    r' = rtoEnvCode r stack
    return' :: RegisterP -> RegisterR -> Stack -> RegisterR
    return' p r' (Stack stack) = case r' of 
        Nothing -> Nothing
        _       -> (R (StackAdress $ Just(r-4)))
   

{-call :: RegisterB -> RegisterC -> RegisterP -> Env -> Stack -> RegisterB ->
call b c p (Stack stack) env = case c of 
    ctoEnvCode c == nil -> (B True) (p+1)
    ctoEnvCode c == _ -> (P (nextC env))

backtrack :: RegisterB -> RegisterC -> RegisterR -> RegisterT -> Stack -> Stack
backtrack p (Stack stack) = case p of-}
