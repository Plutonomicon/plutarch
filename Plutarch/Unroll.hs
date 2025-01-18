{- |
Unrolling a recursive function involves explicitly laying out some or all of the recursive steps, rather than relying on recursion via a fixed-point combinator.
In UPLC, the typical `pfix` implementation uses a Y-combinator under the hood. Each recursive step incurs additional evaluation costs (CPU and memory) due to
the execution of the Y-combinator. By eliminating these costs in each recursive step, unrolled functions reduce execution overhead. However, since each recursive
step is explicitly laid out, unrolled functions consume more script size.

There are various unrolling strategies available. It is important to carefully study the implications of each strategy, as they may impose different requirements,
such as hard limit on recursion depth.

Generally for all strategies, calling the "recursion step"--that is @r@ in @fix $ \r a b ... -> ...@--multiple times will result in exponential increase of the script
size in each step of unrolling. This is because everytime the recursion step is called, it generates UPLC code for that specific branch(and all the subsequent branches
down the unrolling!). To prevent this, it is possible to have @plet r $ \r' -> <..r' can be used multiple times..>@.

Also, due to some of the other optimization performed; specifically @pmatch@'s branch detection which requires hashing terms of all subsequent deconstruction branches,
only tail-call recursion would perform fast enough to be used for large unrolling.
-}
module Plutarch.Unroll (punrollBound, punrollBound', punrollUnbound, punrollUnboundWhole) where

import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (Term, (#$), (:-->))

{- |
This is simplified version of @punrollBound'@ without doing additional recursion on Haskell level value.

@since WIP
-}
punrollBound ::
  forall a b s.
  Integer ->
  Term s (a :--> b) ->
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
punrollBound d def f = punrollBound' d (const def) (\g () -> f (g ())) ()

{- |
The first argument specifies the unrolling depth.
The second argument defines the fallback behavior when the recursion depth exceeds the provided unrolling depth.

The fixed-point implementation provided requires a Haskell-level value @c@ and a Plutarch function of type `Term s (a :--> b)`. The functional for the recursion is passed as a Haskell function.
The inclusion of the additional, arbitrary Haskell value (typed @c@) enables further optimization by allowing pre-computation of constant values that depend only on the recursion depth.

This function will be used in a very niche situations. Using Haskell-level value for constant replacement is only practical on a single branch recursion with constant value that needs to be added
on each step. @plength@ is one of the niche use case.

@since WIP
-}
punrollBound' ::
  forall a b c s.
  Integer ->
  (c -> Term s (a :--> b)) ->
  ((c -> Term s (a :--> b)) -> c -> Term s (a :--> b)) ->
  c ->
  Term s (a :--> b)
punrollBound' 0 def _ c = def c
punrollBound' d def f c = f (punrollBound' (d - 1) def f) c

{- |
Unroll given amount of steps, and for rest, uses `pfix` to support unbound recursion.

@since WIP
-}
punrollUnbound :: forall a b s. Integer -> (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
punrollUnbound 0 f = pfix #$ plam f
punrollUnbound d f = f (punrollUnbound (d - 1) f)

{- |
Uses `pfix` to recurse unrolled function itself. Unlike @punrollUnbound@, this function uses unrolled instructions
within `pfix` recursions.

This should perform better than @punrollUnbound@ when a function requires a large recursion depth.

@since WIP
-}
punrollUnboundWhole :: forall a b s. Integer -> (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
punrollUnboundWhole d f = pfix #$ plam $ \r -> punrollBound d r f
