module NumSimple where

import Data.List

data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

data SymbolicManip a = Number a | Symbol String | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
    | UnaryArity String (SymbolicManip a) deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArity "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

instance Fractional a => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance Floating a => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArity "exp" a
    log a = UnaryArity "log" a
    sqrt a = UnaryArity "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArity "sin" a
    cos a = UnaryArity "cos" a
    tan a = UnaryArity "tan" a
    asin a = UnaryArity "asin" a
    acos a = UnaryArity "acos" a
    atan a = UnaryArity "atan" a
    sinh a = UnaryArity "sinh" a
    cosh a = UnaryArity "cosh" a
    tanh a = UnaryArity "tanh" a
    asinh a = UnaryArity "asinh" a
    acosh a = UnaryArity "acosh" a
    atanh a = UnaryArity "atanh" a

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) = let simpleParen :: (Show a, Num a) => SymbolicManip a -> String
                                      simpleParen (Number x) = prettyShow (Number x)
                                      simpleParen (Symbol x) = prettyShow (Symbol x)
                                      simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
                                      simpleParen x@(UnaryArity _ _) = prettyShow x



                                      pa = simpleParen a
                                      pb = simpleParen b
                                      pop = op2str op
                                  in pa ++ pop ++ pb

prettyShow (UnaryArity opstr a) = opstr ++ "(" ++ show a ++ ")"

instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = let toList (Number x) = [show x]
                toList (Symbol x) = [x]
                toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
                toList (UnaryArity op a) = toList a ++ [op]

                join :: [a] -> [[a]] -> [a]
                join delim l = concat (intersperse delim l)
             in join " " (toList i)

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
    in case (op, sa, sb) of
        (Mul, Number 1, b) -> b
        (Mul, a, Number 1) -> a
        (Mul, Number 0, _) -> Number 0
        (Mul, _, Number 0) -> Number 0
        (Div, a, Number 1) -> a
        (Plus, a, Number 0) -> a
        (Plus, Number 0, b) -> b
        (Minus, a, Number 0) -> a
        _ -> BinaryArith op sa sb

data Units a = Units a (SymbolicManip a) deriving Eq

instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

instance (Eq a, Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

instance (Eq a, Floating a) => Floating (Units a) where
    pi = (Units pi (Number 1))
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub)
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua)
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua)
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"

units :: Num z => z -> String -> Units z
units a b = Units a (Symbol b)

dropUnits :: Num z => Units z -> z
dropUnits (Units x _) = x

deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

test :: Num a => a
test = 2 * 5 + 3
