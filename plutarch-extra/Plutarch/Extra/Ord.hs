-- Needed to 'link' Ordering and POrdering
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Ord (
  -- * Types
  POrdering (..),
  PComparator,

  -- * Functions

  -- ** Creating comparators
  pfromOrd,
  pfromOrdBy,

  -- ** Transforming comparators
  pmapComparator,
  preverseComparator,

  -- ** Using comparators

  -- *** Basic
  pcompareBy,
  pequateBy,
  pleqBy,
  pgeqBy,

  -- *** Sortedness checking
  pisSortedBy,

  -- *** Uniqueness checking
  pallUnique,
  pallUniqueBy,
  ptryAllUnique,
  ptryAllUniqueBy,

  -- *** Sorted merging
  ptryMerge,
  ptryMergeBy,

  -- *** Sorting
  psort,
  psortBy,

  -- *** Nubbing
  pnubSort,
  pnubSortBy,
) where

import Data.Semigroup (Semigroup (stimes), stimesIdempotentMonoid)
import Plutarch.Extra.List (precListLookahead)
import Plutarch.Extra.Maybe (ptraceIfNothing)
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Lift (
  PConstantDecl (
    PConstantRepr,
    PConstanted,
    pconstantFromRepr,
    pconstantToRepr
  ),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.List (pconvertLists)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

{- | A representation of a comparison at the Plutarch level. Equivalent to
 'Ordering' in Haskell.
-}
data POrdering (s :: S)
  = PLT
  | PEQ
  | PGT
  deriving stock (Show, Generic)
  deriving anyclass (PShow, PPartialOrd, POrd, PEq)

instance PUnsafeLiftDecl POrdering where
  type PLifted POrdering = Ordering

instance PConstantDecl Ordering where
  type PConstantRepr Ordering = Integer
  type PConstanted Ordering = POrdering
  pconstantToRepr = \case
    LT -> 0
    EQ -> 1
    GT -> 2
  pconstantFromRepr = \case
    0 -> pure LT
    1 -> pure EQ
    2 -> pure GT
    _ -> Nothing

instance PlutusType POrdering where
  type PInner POrdering = PInteger
  pcon' = \case
    PLT -> 0
    PEQ -> 1
    PGT -> 2
  pmatch' x f =
    pif
      (x #== 0)
      (f PLT)
      ( pif
          (x #== 1)
          (f PEQ)
          (f PGT)
      )

instance Semigroup (Term s POrdering) where
  x <> y = pif (pto x #< 2) (punsafeCoerce $ pto x * pto y) x
  stimes = stimesIdempotentMonoid

instance Monoid (Term s POrdering) where
  mempty = pcon PEQ

data PComparator (a :: PType) (s :: S) = PComparator
  { pcomparatorEq :: Term s (a :--> a :--> PBool)
  , pcomparatorLe :: Term s (a :--> a :--> PBool)
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PComparator a) where
  type DPTStrat _ = PlutusTypeScott

{- | Given a type with a 'POrd' instance, construct a 'PComparator' from that
 instance.
-}
pfromOrd ::
  forall (a :: PType) (s :: S).
  (POrd a) =>
  Term s (PComparator a)
pfromOrd =
  pcon . PComparator (phoistAcyclic $ plam (#==)) $
    phoistAcyclic (plam (#<=))

{- | As 'pfromOrd', but instead uses a projection function into the 'POrd'
 instance to construct the 'PComparator'. Allows other \'-by\' behaviours.
-}
pfromOrdBy ::
  forall (a :: PType) (b :: PType) (s :: S).
  (POrd a) =>
  Term s ((b :--> a) :--> PComparator b)
pfromOrdBy = phoistAcyclic $
  plam $ \f ->
    pmapComparator # f # pfromOrd @a

{- | Given a projection from a type to another type which we have a
 'PComparator' for, construct a new 'PComparator'.
-}
pmapComparator ::
  forall (a :: PType) (b :: PType) (s :: S).
  Term s ((b :--> a) :--> PComparator a :--> PComparator b)
pmapComparator = phoistAcyclic $
  plam $ \f cmp ->
    pmatch cmp $ \(PComparator peq ple) ->
      pcon . PComparator (plam $ \x y -> peq # (f # x) # (f # y)) $
        plam $ \x y -> ple # (f # x) # (f # y)

-- | Reverses the ordering described by a 'PComparator'.
preverseComparator ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> PComparator a)
preverseComparator = phoistAcyclic $
  plam $ \cmp ->
    pmatch cmp $ \(PComparator peq ple) ->
      pcon . PComparator peq $ plam $ \x y -> ple # y # x

-- | \'Runs\' a 'PComparator'.
pcompareBy ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> a :--> a :--> POrdering)
pcompareBy = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator peq ple) ->
      pif
        (peq # x # y)
        (pcon PEQ)
        ( pif
            (ple # x # y)
            (pcon PLT)
            (pcon PGT)
        )

-- | Uses a 'PComparator' for an equality check.
pequateBy ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> a :--> a :--> PBool)
pequateBy = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator peq _) -> peq # x # y

-- | Uses a 'PComparator' for a less-than-or-equals check.
pleqBy ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> a :--> a :--> PBool)
pleqBy = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator _ ple) -> ple # x # y

-- | Uses a 'PComparator' for a greater-than-or-equals check.
pgeqBy ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> a :--> a :--> PBool)
pgeqBy = phoistAcyclic $
  plam $ \cmp x y ->
    pmatch cmp $ \(PComparator peq ple) ->
      pif
        (peq # x # y)
        (pcon PTrue)
        (pnot #$ ple # x # y)

{- | Verifies that a list-like structure is ordered according to the
 'PComparator' argument.
-}
pisSortedBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> PBool)
pisSortedBy = phoistAcyclic $
  plam $ \cmp ->
    precListLookahead (go cmp) (const (pconstant True)) (pconstant True)
  where
    go ::
      forall (s' :: S).
      Term s' (PComparator a) ->
      Term s' (a :--> ell a :--> PBool) ->
      Term s' a ->
      Term s' a ->
      Term s' (ell a) ->
      Term s' PBool
    go cmp self h peek rest =
      pif
        (pleqBy # cmp # h # peek)
        (self # peek # rest)
        (pconstant False)

{- | Verifies that a list-like structure is both ordered (by the 'POrd'
  instance it's full of) and has no duplicates (by the 'PEq' instance
  it's full of).  This can give any of the following results:

 * 'PNothing' if the structure is found to be unsorted;
 * 'PJust' 'PFalse' if the structure contains a duplicate; or
 * 'PJust' 'PTrue' if it is both sorted and contains no duplicates.
-}
pallUnique ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (POrd a, PElemConstraint ell a, PListLike ell) =>
  Term s (ell a :--> PMaybe PBool)
pallUnique = phoistAcyclic $ plam (pallUniqueBy # pfromOrd @a #)

{- | As 'pallUnique', but using a custom 'PComparator' to verify order and
 equality.
-}
pallUniqueBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> PMaybe PBool)
pallUniqueBy = phoistAcyclic $
  plam $ \cmp ->
    precListLookahead (go cmp) (const success) success
  where
    success :: forall (s' :: S). Term s' (PMaybe PBool)
    success = pcon . PJust . pconstant $ True
    go ::
      forall (s' :: S).
      Term s' (PComparator a) ->
      Term s' (a :--> ell a :--> PMaybe PBool) ->
      Term s' a ->
      Term s' a ->
      Term s' (ell a) ->
      Term s' (PMaybe PBool)
    go cmp self h peek t = pmatch (pcompareBy # cmp # h # peek) $ \case
      PLT -> self # peek # t
      PEQ -> pcon . PJust . pconstant $ False
      PGT -> pcon PNothing

{- | As 'pallUnique', but errors if the list-like argument is found to be
 unsorted instead of returning 'PNothing'.
-}
ptryAllUnique ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (POrd a, PElemConstraint ell a, PListLike ell) =>
  Term s (ell a :--> PBool)
ptryAllUnique = phoistAcyclic $ plam (ptryAllUniqueBy # pfromOrd @a #)

{- | As 'pallUniqueBy', but errors if the list-like argument is found to be
 unsorted instead of returning 'PNothing'.
-}
ptryAllUniqueBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> PBool)
ptryAllUniqueBy = phoistAcyclic $
  plam $ \cmp xs ->
    ptraceIfNothing "ptryAllUniqueBy: argument is unordered" $ pallUniqueBy # cmp # xs

{- | Merge two list-like structures, whose contents are sorted by the 'POrd'
 instance for their contents, into one sorted list-like structure. This will
 error if it finds that one of the list-like structures given to it as an
 argument is not sorted.
-}
ptryMerge ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (POrd a, PElemConstraint ell a, PListLike ell) =>
  Term s (ell a :--> ell a :--> ell a)
ptryMerge = phoistAcyclic $ plam (ptryMergeBy # pfromOrd @a #)

{- | As 'ptryMerge', but instead of using 'POrd' to determine sorting, uses a
 custom 'PComparator'. Will error out if one of the list-like structures given
 as an argument is not sorted according to the custom 'PComparator'.
-}
ptryMergeBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> ell a :--> ell a)
ptryMergeBy = phoistAcyclic $
  plam $ \cmp xs ys ->
    phandleList xs (passertSorted # cmp # ys) $ \x xs' ->
      phandleList ys (passertSorted # cmp # xs) $ \y ys' ->
        (pfix #$ plam $ go cmp) # x # xs' # y # ys'
  where
    go ::
      forall (s' :: S).
      Term s' (PComparator a) ->
      Term s' (a :--> ell a :--> a :--> ell a :--> ell a) ->
      Term s' a ->
      Term s' (ell a) ->
      Term s' a ->
      Term s' (ell a) ->
      Term s' (ell a)
    go cmp self x xs y ys =
      pif
        (pleqBy # cmp # x # y)
        ( pcons # x #$ phandleList xs (passertSortedLookahead # cmp # y # ys) $ \x' xs' ->
            pif
              (pleqBy # cmp # x # x')
              (self # x' # xs' # y # ys)
              unorderedError
        )
        ( pcons # y #$ phandleList ys (passertSortedLookahead # cmp # x # xs) $ \y' ys' ->
            pif
              (pleqBy # cmp # y # y')
              (self # x # xs # y' # ys')
              unorderedError
        )

{- | Sort a list-like by the 'POrd' instance of its contents.
 This uses a [merge sort](https://en.wikipedia.org/wiki/Merge_sort)
 implementation, which is also
 [stable](https://en.wikipedia.org/wiki/Sorting_algorithm#Stability). As this
 is a comparison sort, it requires a linearithmic ($n \log(n)$) number of
 comparisons to complete.
-}
psort ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (POrd a, PElemConstraint ell a, PListLike ell) =>
  Term s (ell a :--> ell a)
psort = phoistAcyclic $ plam (psortBy # pfromOrd @a #)

-- | As 'psort', but uses a custom 'PComparator' for order comparisons.
psortBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> ell a)
psortBy = phoistAcyclic $
  plam $ \cmp xs ->
    pmergeAllUnsafe
      cmp
      (const id)
      pmergeUnsafe
      #$ pmergeStart_2_3_4
      # cmp
      # xs

{- | As 'psort', but also throws out all duplicates according to the 'PEq'
 instance of its argument's contents.
-}
pnubSort ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (POrd a, PElemConstraint ell a, PListLike ell) =>
  Term s (ell a :--> ell a)
pnubSort = phoistAcyclic $ plam (pnubSortBy # pfromOrd @a #)

{- | As 'pnubSort', but uses a custom 'PComparator' for both ordering and
 equality.
-}
pnubSortBy ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> ell a)
pnubSortBy = phoistAcyclic $
  plam $ \cmp xs ->
    pmergeAllUnsafe
      cmp
      (\cmp' xs' -> pnubUnsafe # cmp' # xs')
      pmergeUnsafeNoDupes
      #$ pmergeStart_2_3_4
      # cmp
      # xs

-- Helpers

-- Merges nested PLists and repacks into a list-like of your choice, given a
-- method of merging the PLists themselves.
--
-- Only call this if you are _sure_ that the 'inner' PLists are sorted according
-- to the argument comparator!
pmergeAllUnsafe ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a) ->
  (forall (s' :: S). Term s' (PComparator a) -> Term s' (PList a) -> Term s' (PList a)) ->
  (forall (s' :: S). Term s' (PComparator a) -> Term s' (PList a) -> Term s' (PList a) -> Term s' (PList a)) ->
  Term s (PList (PList a) :--> ell a)
pmergeAllUnsafe cmp whenSingleton whenMerging = plam $ \xs -> pconvertLists #$ pfix # plam go # xs
  where
    go ::
      Term s (PList (PList a) :--> PList a) ->
      Term s (PList (PList a)) ->
      Term s (PList a)
    go self xs = phandleList xs pnil $ \y ys ->
      phandleList ys (whenSingleton cmp y) $ \_ _ ->
        self #$ pmergePairs # xs
    pmergePairs :: Term s (PList (PList a) :--> PList (PList a))
    pmergePairs = pfix #$ plam $ \self xs ->
      phandleList xs pnil $ \x' xs' ->
        phandleList xs' xs $ \x'' xs'' ->
          pcons # whenMerging cmp x' x'' #$ self # xs''

-- Merges two PLists, leaving duplicates in place.
--
-- Only call this if you are _sure_ that the PLists are sorted according to the
-- argument comparator!
pmergeUnsafe ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s (PList a) ->
  Term s (PList a) ->
  Term s (PList a)
pmergeUnsafe cmp xs ys =
  phandleList xs ys $ \x' xs' ->
    (pfix #$ plam $ go) # x' # xs' # ys
  where
    go ::
      Term s (a :--> PList a :--> PList a :--> PList a) ->
      Term s a ->
      Term s (PList a) ->
      Term s (PList a) ->
      Term s (PList a)
    go self x' xs' ys' =
      phandleList ys' (pcons # x' # xs') $ \y'' ys'' ->
        pif
          (pleqBy # cmp # x' # y'')
          (pcons # x' #$ self # y'' # ys'' # xs')
          (pcons # y'' #$ self # x' # xs' # ys'')

-- Merges two PLists, throwing out duplicates.
--
-- Only call this if you are _sure_ that the PLists are sorted according to the
-- argument comparator!
pmergeUnsafeNoDupes ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s (PList a) ->
  Term s (PList a) ->
  Term s (PList a)
pmergeUnsafeNoDupes cmp xs ys =
  phandleList xs (pnubUnsafe # cmp # ys) $ \x' xs' ->
    (pfix #$ plam $ go) # x' # xs' # ys
  where
    go ::
      Term s (a :--> PList a :--> PList a :--> PList a) ->
      Term s a ->
      Term s (PList a) ->
      Term s (PList a) ->
      Term s (PList a)
    go self x' xs' ys' =
      phandleList ys' (pnubUnsafe # cmp #$ pcons # x' # xs') $ \y'' ys'' ->
        pmatch (pcompareBy # cmp # x' # y'') $ \case
          PLT -> pcons # x' #$ self # y'' # ys'' # xs'
          PEQ -> self # y'' # ys'' # xs'
          PGT -> pcons # y'' #$ self # x' # xs' # ys''

-- Removes all duplicates from a sorted list.
--
-- Only call this if you are _sure_ that the PLists are sorted according to the
-- argument comparator!
pnubUnsafe ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a :--> PList a :--> PList a)
pnubUnsafe = phoistAcyclic $
  plam $ \cmp ->
    precListLookahead (go cmp) (psingleton #) pnil
  where
    go ::
      forall (s' :: S).
      Term s' (PComparator a) ->
      Term s' (a :--> PList a :--> PList a) ->
      Term s' a ->
      Term s' a ->
      Term s' (PList a) ->
      Term s' (PList a)
    go cmp self h peek t =
      pif
        (pequateBy # cmp # h # peek)
        (self # peek # t)
        (pcons # h #$ self # peek # t)

-- Breaks the argument into sorted chunks; currently, these range in size from 2
-- to 4, with preference for larger chunks. To speed up the chunk sorting, we
-- use sorting networks.
pmergeStart_2_3_4 ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> PList (PList a))
pmergeStart_2_3_4 = phoistAcyclic $
  pfix #$ plam $ \self cmp ->
    pmatchList pnil $
      \_0 -> pmatchList (plist [psing _0]) $
        \_1 -> pmatchList (plist [psort2 cmp _0 _1]) $
          \_2 -> pmatchList (plist [psort3 cmp _0 _1 _2]) $
            \_3 rest -> pcon $ PSCons (psort4 cmp _0 _1 _2 _3) (self # cmp # rest)
  where
    pmatchList ::
      forall (r :: PType) (s' :: S).
      Term s' r ->
      (Term s' a -> Term s' (ell a) -> Term s' r) ->
      Term s' (ell a) ->
      Term s' r
    pmatchList = flip pelimList

-- Unhoisted Foldable lifter
plist ::
  forall (a :: PType) (f :: Type -> Type) (s :: S).
  (Foldable f) =>
  f (Term s a) ->
  Term s (PList a)
plist = foldr (\x -> pcon . PSCons x) (pcon PSNil)

-- Unhoisted singleton maker
psing ::
  forall (a :: PType) (s :: S).
  Term s a ->
  Term s (PList a)
psing x = plist [x]

-- Two-item sorting network; basically a comparison.
psort2 ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s a ->
  Term s a ->
  Term s (PList a)
psort2 cmp _0 _1 =
  pswap cmp _0 _1 $ \_0 _1 ->
    plist [_0, _1]

-- Three-item sorting network. The layers are:
--

-- * Arg 0 vs arg 2

-- * Arg 0 vs arg 1

-- * Arg 1 vs arg 2
psort3 ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s a ->
  Term s a ->
  Term s a ->
  Term s (PList a)
psort3 cmp _0 _1 _2 =
  pswap cmp _0 _2 $ \_0 _2 ->
    pswap cmp _0 _1 $ \_0 _1 ->
      pswap cmp _1 _2 $ \_1 _2 ->
        plist [_0, _1, _2]

-- Four-item sorting network. The layers are:
--

-- * Arg 0 vs arg 2 and arg 1 versus arg 3

-- * Arg 0 vs arg 1 and arg 2 versus arg 3

-- * Arg 1 versus arg 2

--
-- We perform the layer parallelism sequentially; it doesn't affect the
-- semantics, it just makes us sad because it's slow.
psort4 ::
  forall (a :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s a ->
  Term s a ->
  Term s a ->
  Term s a ->
  Term s (PList a)
psort4 cmp _0 _1 _2 _3 =
  pswap cmp _0 _2 $ \_0 _2 ->
    pswap cmp _1 _3 $ \_1 _3 ->
      (\f -> f # cmp # _0 # _1 # _2 # _3) $
        phoistAcyclic $
          plam $
            \cmp' _0 _1 _2 _3 ->
              pswap cmp' _0 _1 $ \_0 _1 ->
                pswap cmp' _2 _3 $ \_2 _3 ->
                  pswap cmp' _1 _2 $ \_1 _2 ->
                    plist [_0, _1, _2, _3]

-- Runs a 'sorting network layer' driven by a given PComparator. Written in CPS
-- style for efficiency.
--
-- Note
--
-- Chaining this leads to duplication in each 'pif' branch, which can cause
-- script sizes to blow up. Specifically, the total number of 'pifs' will be
-- 2 ^ (n - 1), where n is the length of the 'swap chain'.
--
-- To reduce blowup, you want to 'cut' your swap chain into larger 'stages',
-- separated by a lambda. You can see an example of how to do this in the psort4
-- function in this module.
--
-- Three to four swaps per 'stage' is a good compromise. Using more lambdas
-- increases execution costs, as each lambda needs to be hoisted, or you still
-- end up with duplication costs. In order to hoist such a lambda, it needs to
-- receive the entire list as an argument, as otherwise, it would need access to
-- out-of-scope variables, which is prevented by the type system.
--
-- Using 'plet' instead of hoisting doesn't really help, unless the whole
-- sorting network is only called once, since you pay almost the same cost for
-- each execution as hoisting would require.
pswap ::
  forall (a :: PType) (r :: PType) (s :: S).
  Term s (PComparator a) ->
  Term s a ->
  Term s a ->
  (Term s a -> Term s a -> Term s r) ->
  Term s r
pswap cmp x y cont = pif (pleqBy # cmp # x # y) (cont x y) (cont y x)

-- pelimList with the list-like first, and handles the 'nil case' before the
-- 'cons' case
phandleList ::
  forall (a :: PType) (r :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (ell a) ->
  Term s r ->
  (Term s a -> Term s (ell a) -> Term s r) ->
  Term s r
phandleList xs whenNil whenCons = pelimList whenCons whenNil xs

-- ensures the argument is sorted by the comparator, erroring if not
passertSorted ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> ell a :--> ell a)
passertSorted = phoistAcyclic $
  plam $ \cmp xs ->
    phandleList xs xs $ \x' xs' ->
      passertSortedLookahead # cmp # x' # xs'

-- as passertSorted, but with the 'lookahead' already done
passertSortedLookahead ::
  forall (a :: PType) (ell :: PType -> PType) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (PComparator a :--> a :--> ell a :--> ell a)
passertSortedLookahead = phoistAcyclic $
  plam $ \cmp x xs ->
    pif
      (pisSortedBy # cmp # xs)
      ( phandleList xs (psingleton # x) $ \x' _ ->
          pif
            (pleqBy # cmp # x # x')
            (pcons # x # xs)
            unorderedError
      )
      unorderedError

unorderedError :: forall (a :: PType) (s :: S). Term s a
unorderedError = ptraceError "ptryMergeBy: argument list-like out of order"
