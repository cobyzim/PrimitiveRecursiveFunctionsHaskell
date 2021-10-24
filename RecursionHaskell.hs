{-
Program that computes addition, subtraction, multiplication, exponentials, and equality using only primitive recursive functions
-}

module CS475Assign4 where

    import System.IO
    import Debug.Trace

    type PN = [Char]

    z :: PN -> PN
    z x = "0"

    --I think its important to note that the def for s x can be used with any PN num after s, but we are assuming that
    --users know to use "0"
    s :: PN -> PN
    s "0" = ['S', '0']
    s x = ('S':x)

    p :: PN -> PN
    p "0" = "0"
    p ('S':n) = pi_1_2 (n, p(n))

    pi_1_1 (x1) = x1
    pi_1_2 (x1, _) = x1
    pi_2_2 (_, x2) = x2
    pi_1_3 (x1, _, _) = x1
    pi_2_3 (_, x2, _) = x2
    pi_3_3 (_, _, x3) = x3

    -- For the f_mult so that we can get "0" for the mult base case
    pi_2_1 (x2) = "0"

    -- For the g_mult so that x and add(x, n) can be used in the composition function at the same time
    pi_12_3 (x1, x2, _) = (x1, x2)

     -- For the f_exp so that we get "S0" for the base case
    pi_0_1 (x1) = "S0"

    -- For the g_exp so that exp(n, x) and x can be used in the composition function at the same time
    pi_13_3 (x1, _, x3) = (x1, x3)

    -- Trace functions
    sh0 :: PN -> PN
    sh0 x = trace ("- " ++ x) x

    sh1 :: PN -> PN
    sh1 x = trace ("---- " ++ x) x

    recursDef :: (PN -> PN) -> ((PN, PN, PN) -> PN) -> (PN, PN) -> PN
    recursDef f g = go
        where go (x, "0") = sh0 $ f (x) -- Base case
              go (x, ('S':n)) = sh1 $ g (x, go (x, n), n) --Inductive case

    add :: (PN, PN) -> PN
    add = recursDef f_add g_add

    mult :: (PN, PN) -> PN
    mult = recursDef f_mult g_mult

    f_add = pi_1_1
    g_add = s . pi_2_3

    f_mult = pi_2_1
    g_mult = add . pi_12_3

    sub :: (PN, PN) -> PN
    sub = recursDef f_sub g_sub

    f_sub = pi_1_1
    g_sub = p . pi_2_3

    -- Created a new recursive definition to make the expo function simpler
    recursExpDef :: (PN -> PN) -> ((PN, PN, PN) -> PN) -> (PN, PN) -> PN
    recursExpDef f g = go
        where go ("0", x) = sh0 $ f (x)
              go (('S':n), x) = sh1 $ g (go (n, x), n, x)

    expo :: (PN, PN) -> PN
    expo = recursExpDef f_expo g_expo

    f_expo = pi_0_1
    g_expo = mult . pi_13_3

    eq(x, y) = case add ( sub (x, y), sub (y, x)) of
                    "0" -> "0"
                    ('S':n) -> "S0"