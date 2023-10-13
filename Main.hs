-- estarei optatando por usar Zero e seus sucessores
-- http://learnyouahaskell.com/higher-order-functions
data Nat = Zero | Suc Nat
    deriving (Show)

--  1 - 4
um, dois, tres, quatro :: Nat
um = Suc Zero
dois = Suc um
tres = Suc dois
quatro = Suc tres

--  Nat para Integer
nat2integer :: Nat -> Integer
nat2integer Zero = 0
nat2integer (Suc n) = 1 + nat2integer n -- vai executar N vezes até n ter mais recursção, nao leva em consideração antigos

-- integer para Nat
integer2nat :: Integer -> Nat
integer2nat 0 = Zero
integer2nat n
    | n > 0 = Suc (integer2nat (n - 1)) --- N vezes nat2integer(N) Nn = Suc Zero

 

-- adicao
natAdd :: Nat -> Nat -> Nat
natAdd Zero n = n
natAdd (Suc m) n = Suc (natAdd m n) 

-- subtração (nunca negativo)
natSub :: Nat -> Nat -> Nat
natSub m Zero = m
natSub (Suc m) (Suc n) = natSub m n 

-- multiplicacao naturais
natMul :: Nat -> Nat -> Nat
natMul Zero _ = Zero
natMul _ Zero = Zero
natMul m (Suc n) = natAdd m (natMul m n)

main :: IO ()
main = do

    putStrLn "1 - 4:"
    print um
    print dois
    print tres
    print quatro

    putStrLn "Nat para Integer:"
    print (nat2integer tres)

    putStrLn "Integer para Nat:"
    print (integer2nat 5)

    putStrLn "Adição:"
    print (natAdd dois tres)

    putStrLn "Subtração:"
    print (natSub tres um)
    print (nat2integer (natSub tres um))
    
    
    putStrLn "Multiplicação:"
    print(natMul dois dois)