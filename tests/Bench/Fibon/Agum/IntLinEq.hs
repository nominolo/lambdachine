-- Integer Solutions of Linear Inhomogeneous Equations
--
-- Copyright (C) 2009 John D. Ramsdell
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- |
-- Module      : Algebra.AbelianGroup.IntLinEq
-- Copyright   : (C) 2009 John D. Ramsdell
-- License     : GPL
--
-- Integer Solutions of Linear Inhomogeneous Equations
--
-- A linear equation with integer coefficients is represented as a
-- pair of lists of non-zero integers, the coefficients and the
-- constants.  If there are no constants, the linear equation
-- represented by (c, []) is the homogeneous equation:
--
-- >     c[0]*x[0] + c[1]*x[1] + ... + c[n-1]*x[n-1] = 0
--
-- where n is the length of c.  Otherwise, (c, d) represents the
-- inhomogeneous equation:
--
-- >     c[0]*x[0] + c[1]*x[1] + ... + c[n-1]*x[n-1] = g
--
-- where g = gcd(d[0], d[1], ..., d[m-1]), and m is the length of d.
-- Thus g is the greatest common denominator of the elements of d.
--
-- A solution is a partial map from variables to terms, and a term is
-- a pair of lists of integers, the variable part of the term followed
-- by the constant part.  The variable part may specify variables not
-- in the input.  In other words, the length of the coefficents in the
-- answer may exceed the length of the coefficients in the input.  For
-- example, the solution of
--
-- >     64x - 41y = 1
--
-- is x = -41z - 16 and y = -64z - 25.  The computed solution is read
-- off the list returned as an answer.
--
-- >     intLinEq [64,-41] [1] =
-- >         [(0,([0,0,0,0,0,0,-41],[-16])),
-- >         (1,([0,0,0,0,0,0,-64],[-25]))]
--
-- The algorithm used to find solutions is described in Vol. 2 of The
-- Art of Computer Programming \/ Seminumerical Alorithms, 2nd Ed.,
-- 1981, by Donald E. Knuth, pg. 327.  To show sums, we write
--
-- >     sum[i] c[i]*x[i] for c[0]*x[0] + c[1]*x[1] + ... + c[n-1]*x[n-1].
--
-- The algorithm's initial values are the linear equation (c,d) and an
-- empty substitution s.
--
-- 1.  Let c[i] be the smallest non-zero coefficient in absolute value.
--
-- 2.  If c[i] < 0, multiply c and d by -1 and goto step 1.
--
-- 3.  If c[i] = 1, a general solution of the following form has been
-- found:
--
-- >     x[i] = sum[j] -c'[j]*x[j] + d[k] for all k
--
--  where c' is c with c'[i] = 0.  Use the equation to eliminate x[i]
--  from the range of the current substitution s.  If variable x[i] is
--  in the original equation, add the mapping to substitution s.
--
-- 4.  If c[i] divides every coefficient in c,
--
--     * if c[i] divides every constant in d, divide c and d by c[i]
--       and goto step 3,
--
--     * otherwise fail because there is no solution.
--
-- 5.  Otherwise, eliminate x[i] as above in favor of freshly created
-- variable x[n], where n is the length of c.
--
-- >    x[n] = sum[j] (c[j] div c[i] * x[j])
--
-- Goto step 1 and solve the equation:
--
-- >    c[i]*x[n] + sum[j] (c[j] mod c[i])*x[j] = d[k] for all k
{-# LANGUAGE NoImplicitPrelude #-}
module Bench.Fibon.Agum.IntLinEq
    (LinEq, Subst, intLinEq) where

import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum ()

-- | A linear equation with integer coefficients is represented as a
-- pair of lists of non-zero integers, the coefficients and the
-- constants.
type LinEq = ([Int], [Int])

-- | A solution to a linear equation is a partial map from variables
-- to terms, and a term is a pair of lists of integers, the variable
-- part of the term followed by the constant part.  The variable part
-- may specify variables not in the input.  In other words, the length
-- of the coefficents in the answer may exceed the length of the
-- coefficients in the input.
type Subst = [(Int, LinEq)]

-- | Find integer solutions to a linear equation or fail when there
-- are no solutions.
intLinEq :: Monad m => LinEq -> m Subst
intLinEq (coefficients, constants) =
    intLinEqLoop (length coefficients) (coefficients, constants) []

-- The algorithm used to find solutions is described in Vol. 2 of The
-- Art of Computer Programming / Seminumerical Alorithms, 2nd Ed.,
-- 1981, by Donald E. Knuth, pg. 327.

-- On input, n is the number of variables in the original problem, c
-- is the coefficients, d is the constants, and subst is a list of
-- eliminated variables.
intLinEqLoop :: Monad m => Int -> LinEq -> Subst -> m Subst
intLinEqLoop n (c, d) subst =
    -- Find the smallest non-zero coefficient in absolute value
    let (i, ci) = smallest c in
    case () of
      _ | ci < 0 -> intLinEqLoop n (invert c, invert d) subst
      --  Ensure the smallest coefficient is positive
        | ci == 0 -> fail "bad problem"
      --  Lack of non-zero coefficients is an error
        | ci == 1 ->
      --  A general solution of the following form has been found:
      --    x[i] = sum[j] -c'[j]*x[j] + d[k] for all k
      --  where c' is c with c'[i] = 0.
            return $ eliminate n (i, (invert (zero i c), d)) subst
        | divisible ci c ->
      --  If all the coefficients are divisible by c[i], a solution is
      --  immediate if all the constants are divisible by c[i],
      --  otherwise there is no solution.
            if divisible ci d then
                let c' = divide ci c
                    d' = divide ci d in
                return $ eliminate n (i, (invert (zero i c'), d')) subst
            else
                fail "no solution"
        | otherwise ->
      --  Eliminate x[i] in favor of freshly created variable x[n],
      --  where n is the length of c.
      --    x[n] = sum[j] (c[j] div c[i] * x[j])
      --  The new equation to be solved is:
      --    c[i]*x[n] + sum[j] (c[j] mod c[i])*x[j] = d[k] for all k
            intLinEqLoop n (map (\x -> modInt x ci) c ++ [ci], d) subst'
            where
              subst' = eliminate n (i, (invert c' ++ [1], [])) subst
              c' = divide ci (zero i c)

-- Find the smallest non-zero coefficient in absolute value
smallest :: [Int] -> (Int, Int)
smallest xs =
    foldl f (-1, 0) (zip [0..] xs)
    where
      f (i, n) (j, x)
        | n == 0 = (j, x)
        | x == 0 || abs n <= abs x = (i, n)
        | otherwise = (j, x)

invert :: [Int] -> [Int]
invert t = map negate t

-- Zero the ith position in a list
zero :: Int -> [Int] -> [Int]
zero _ [] = []
zero 0 (_:xs) = 0 : xs
zero i (x:xs) = x : zero (i - 1) xs

-- Eliminate a variable from the existing substitution.  If the
-- variable is in the original problem, add it to the substitution.
eliminate :: Int -> (Int, LinEq) -> Subst -> Subst
eliminate n m@(i, (c, d)) subst =
    if i < n then
        m : map f subst
    else
        map f subst
    where
      f m'@(i', (c', d')) =     -- Eliminate i in c' if it occurs in c'
          case get i c' of
            0 -> m'             -- i is not in c'
            ci -> (i', (addmul ci (zero i c') c, addmul ci d' d))
      -- Find ith coefficient
      get _ [] = 0
      get 0 (x:_) = x
      get i (_:xs) = get (i - 1) xs
      -- addnum n xs ys sums xs and ys after multiplying ys by n
      addmul 1 [] ys = ys
      addmul n [] ys = map (* n) ys
      addmul _ xs [] = xs
      addmul n (x:xs) (y:ys) = (x + n * y) : addmul n xs ys

divisible :: Int -> [Int] -> Bool
divisible small t =
    all (\x -> modInt x small == 0) t

divide :: Int -> [Int] -> [Int]
divide small t =
    map (\x -> divInt x small) t
