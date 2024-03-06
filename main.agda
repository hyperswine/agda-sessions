module main where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

data Greeting : Set where
  hello : Greeting

greet = hello

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main = putStrLn "Hi!!"

-- prove that 1 != 0

-- postulate
--   data ⊥ : Set where
--   data ℕ : Set where
--   zero : ℕ
--   suc : ℕ → ℕ
--   data _≟_ {A : Set} : A → A → Set where
--   yes : ∀ {x} → x ≟ x
--   no : ∀ {x y} → x ≟ y → ⊥
--   infix 4 _≟_
--   data _≤_ : ℕ → ℕ → Set where
--   z≤n : ∀ {n} → zero ≤ n
--   s≤s : ∀ {n m} → n ≤ m → suc n ≤ suc m
--   infix 4 _≤_
--   data _<_ : ℕ → ℕ → Set where
--   z<n : ∀ {n} → zero < suc n
--   s<n : ∀ {n m} → n < m → suc n < suc m
--   infix 4 _<_
--   data _+_ : ℕ → ℕ → Set where
--   z+ : ∀ {n} → zero + n ≡ n
--   s+ : ∀ {n m} → n + m ≡ suc (n + m)
--   infixl 6 _+_
--   data _*_ : ℕ → ℕ → Set where
--   z* : ∀ {n} → zero * n ≡ zero
--   s* : ∀ {n m} → n * m + n ≡ n * suc m
--   infixl 7 _*_
--   data _^_ : ℕ → ℕ → Set where
--   z^ : ∀ {n} → suc n ^ zero ≡ suc zero
--   s^ : ∀ {n m} → suc n ^ suc m ≡ suc n * suc n ^ m
--   infixr 8 _^_
--   data _÷_ : ℕ → ℕ → Set where
--   z÷ : ∀ {n} → zero ÷ suc n ≡ zero
--   s÷ : ∀ {n m} → suc n ÷ suc m ≡ suc n ÷ m + suc n * suc n ^ m
--   infixl 7 _÷_
--   data _mod_ : ℕ → ℕ → Set where
--   zmod : ∀ {n} → zero mod suc n ≡ zero
--   smod : ∀ {n m} → suc n mod suc m
