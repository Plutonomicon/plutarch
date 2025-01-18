# Revision history for plutarch

# WIP

## Added

* `pmax` and `pmin` as new methods of `POrd`
* `#>` and `#>=` as argument-flipping versions of `#<` and `#<=`
* `pallBS` to `Plutarch.ByteString` (originally from `plutarch-extra`)
* `pisHexDigit` to `Plutarch.String` (originally from `plutarch-extra`)
* `preverse` and `pcheckSorted` to `Plutarch.List` (originally from
  `plutarch-extra`)
* `ptraceIfNothing`, `pisJust`, `pmaybe`, `pfromMaybe`, `pjust`, `pnothing`,
  `pAssertPJust` to `Plutarch.Maybe` (originally from `plutarch-extra`)
* `pexpectJustC` to `Plutarch.TermCont` (originally from `plutarch-extra`)
* `PCountable` and `PEnumerable` type classes, as well as instances
* `PByte` type as a limited Plutarch-level equivalent to `Word8`
* `PLogicSemantics`, and construction functions, to help use of logical
   `PByteString` operations
* `pandBS`, `porBS`, `pxorBS`, `pcomplementBS` mirroring CIP-122 operations
* `pzeroesBS`, `ponesBS`, `preplicateBS`, as wrappers for CIP-122's
  `ReplicateByte`
* `compileOptimized` in `Plutarch.Internal`, to optimize the generated UPLC
* `PBitString` and associated functionality for CIP-122 and CIP-123 operations
  on bits, in `Plutarch.BitString`
* `PEitherData`, a `Data`-encoded counterpart to `PEither`, plus some functions
* `Positive` type in `Plutarch.Positive` that is Haskell level equivalent of
  `PPositive`
* `PUnsafeLiftDecl` and `PConstantDecl` instances for `PPositive`
* `evalScriptUnlimited` to `Plutarch.Evaluate` as unrestricted version of `evalScript`
* `pmapMaybe` to `Plutarch.Maybe`
* `PLiftable` type class
* `Plutarch.Builtin.Bool` module
* `Plutarch.Internal.Eq` module
* `Plutarch.Internal.Ord` module
* `pif'` to `Plutarch.Prelude`
* `pcond` as a Plutarch equivalent to multi-way if
* `PLiftable PRational` instance
* `PDataFields`, `DerivePDataLiftable`, `PDataNewtype` now exported from the prelude
* New methods for `Data` and Scott encoding derivation
* `optimizeTerm` for separate optimization via UPLC from compilation
* New numerical hierarchy in `Plutarch.Internal.Numeric`, plus new instances
* `PNatural` type, corresponding to the Haskell `Natural`
* Support for SoP encoding of data
* `PSemigroup` and `PMonoid`, as improved Plutarch versions of `Semigroup` and
  `Monoid`
* Unrolling utilities--`punrollBound`, `punrollUnbound`, and `punrollUnboundWhole`--is added to `Plutarch.Unroll`
* `evalTerm'` is added to `Plutarch.Evaluate`

## Changed

* `pconsBS` now takes a `PByte` argument instead of a `PInteger` one
* `pindexBS` now returns a `PByte` instead of a `PInteger`
* `pexpModInteger` is now in `Plutarch.Integer`
* `PMaybeData` no longer uses `PDataRecord`
* `Plutarch.Internal` is now `Plutarch.Internal.Term` to better reflect its
  actual contents
* `PBool` definition is now in `Plutarch.Builtin.Bool`
* `PEq` type class definition is now in `Plutarch.Internal.Eq`
* `PPartialOrd` and `POrd` type class definitions are now in
  `Plutarch.Internal.Ord`
* `pif`, `pif'`, `pand`, `pand'`, `por`, `por'`, `pnot`, `#&&`, `#||` are
   now in `Plutarch.Builtin.Bool`
* `Term s PRational` is now `Fractional` directly, instead of by way of
  `PFractional`
* `Plutarch.Num` is now `Plutarch.Internal.Numeric`
* `PIntegral` type class is now in `Plutarch.Internal.Numeric`
* `Plutarch.Integer` is now `Plutarch.Builtin.Integer`
* `#<`, `#<=`, `#>=`, `#>` are now part of `POrd`
* `PPositive` (and `Positive`) are now exported from the prelude, along with
  some functionality
* `PEither`, `PPair`, `PMaybe` and `PList` use SOP encoding instead of Scott

## Removed

* `plutarch-extra`, as all its functionality has been folded into Plutarch
  itself
* `pbyteStr` (as it's deprecated)
* `Plutarch.Bitwise` module, as its functionality has been superseded by more
  type-safe operations in `Plutarch.ByteString` and  `Plutarch.BitString`
* `PUnsafeLiftDecl` and `PConstantDecl` as they are replaced by `PLiftable`
* `Plutarch` module, as it served no useful purpose and was just confusing
* `PType` synonym (use `S -> Type` honestly instead)
* `PSBool` and functionality (now in `plutarch-ledger-api`)
* `PFractional` type class (only one instance, unlikely to ever have more)
* `PIsData PRational` instance (made no sense)
* `PPartialOrd` (all its functionality is now in `POrd`)
* `Plutarch.FFI` module
* `PNum` and `PIntegral` (replaced by new numerical hierarchy)

### Fixed

* Bug in `ppredecessorN` for `PPosixTime` where order of subtraction was flipped
* Bugs in `pintersection` and `phull` that assigned wrong open/close bounds
* Bug in `pafter` that will give opposite result when comparing against infinities

# 1.9.0 - 25-09-2024

## Added

* Cryptographic hashing utilities:- `pripemd_160`, `pkeccak_256`, and `pblake2b_224` to `Plutarch.Crypto`
* PlutusV3 BLS primitives to `Plutarch.BLS`
* PlutusV3 Bitwise primitives to `Plutarch.Bitwise`
* `unsafeEvalTerm`  to `Plutarch.Evaluate`
* `PCountable` and `PEnumerable` type classes in `Plutarch.Enum`

## Changed

* Bumped `plutus-core` version to `1.33.0.0`
* Updated getArity mapping to handle the new builtins in `Plutarch.Internal`
* Updated `printScript` in `Plutarch.Internal.Other` to new PlutusCore version.

# 1.8.1 - 11-07-2024

## Added

* `applyArguments` to `Plutarch.Evaluate`

# 1.8.0 - 24-06-2024

## Changed

* Bumped `plutus-core` version to `1.30.0.0`

# 1.7.0 - 11-06-2024

## Added

* `PDataNewtype` derivation aid
* `PTryFrom PData PBool` instance

## Changed

* Bumped `plutus-core` version to `1.29.0.0`

# 1.6.0 - 27-05-2024

## Added

* `ptraceInfo` and `ptraceDebug`, which allow tracing only when a particular log
  level is active.
* `logLevel` to get the `LogLevel` of a `Config`.
* `LogLevel` to indicate what level of logging we'd like to run with.
* `ptraceInfoShowId`, `ptraceInfoError`, `ptraceInfoIfTrue`, `ptraceInfoIfFalse`
  (and similar for `Debug`), mirroring the deprecated originals, but with the
  logging level included.
* `Eq`, `Show`, `Pretty`, `ToJSON`, `FromJSON` instances for `Config`.
* `Pretty`, `ToJSON`, `FromJSON` instances for `TracingMode`.

## Changed

* `ptrace`, `ptraceShowId`, `ptraceError`, `ptraceIfTrue` and `ptraceIfFalse`
  are now synonyms of `ptraceInfo` (and similar), and also deprecated.
* `Config` now includes a `LogLevel` as well as a `TracingMode`.
* `Config` now has pattern synonyms to make it look like a sum type with two
  arms: `NoTracing` which indicates that we do not trace, and `Tracing` which
  contains a `TracingMode` and a `LogLevel`.
* `TracingMode` no longer includes `NoTracing`, as this has been superseded by
  the new `Config`.
* `tracingMode` now returns in a `Maybe`.
* `Config` is now a `Semigroup` and a `Monoid`, with the second mimicking the
  semantics of its old `Default` instance.
* `TracingMode` is now a `Semigroup` based on generality.
* `TracingMode` is now an `Ord` based on generality.
* `TracingMode` now has `Eq` and `Show` instances.

## Removed

* `Default` instance for `Config`.
* `data-default` direct dependency.

# 1.5.0 - 26-01-2024

## Changed

*  Bump `plutus-core` and `plutus-ledger-api` to `1.20.0.0`

## Removed

* `Plutarch.LedgerApi`, plus all submodules, as these are now under `plutarch-ledger-api`

# 1.4.0

- Renamed `punionWith` and `punionWithData` of `Plutarch.Api.V1.AssocMap` to
  `punionResolvingCollisionsWith` and `punionResolvingCollisionsWithData`, since
  they have been misused for what should have been `pzipWith` and `pzipWithData`.
- Renamed `punionWith` and `punionWithData` of `Plutarch.Api.V1.Value` to
  `punionResolvingCollisionsWith` and `punionResolvingCollisionsWithData`, since
  they have been misused for what should have been `pzipWith` and `pzipWithData`.
- Introduced `pzipWith`, `pzipWithData`, `pzipWithDefault`, `pzipWithDataDefault`,
  `pzipWithDefaults`, `pzipWithDataDefaults`, `pintersectionWith`, and
  `pintersectionWithData` in `Plutarch.Api.V1.AssocMap`. Also introduced the
  types `BothPresentHandler`, `BothPresentHandlerCommutative`,
  `OnePresentHandler`, `MergeHandler` and `SomeMergeHandler` for instructing
  `pzipWith` and `pzipWithData`.
- Added `Commutativity` in `AssocMap` for instructing various `AssocMap` and
  `Value` operations about the commutativity of the given value-merging
  function.
- Renamed `pdifference` in `AssocMap` to `punsortedDifference` (O(n^2)).
  Introduced new `pdifference` with stricter constraints and O(n).
- Added `pleftBiasedUnion` in `Plutarch.Api.V1.AssocMap`.
- Added `pleftBiasedCurrencyUnion` and `pleftBiasedTokenUnion` in
  `Plutarch.Api.V1.Value`.

# 1.3.0

- Support newer Plutus
- Remove most of the Nix code and switch to mlabs-tooling.nix

# 1.2.1

- Exported data constructors instance for `Plutarch.Lift (LiftError)`

# 1.2 (WIP changelog)

- Changed fields of `PTxInfo` whose only representation is data to not be wrapped by `PAsData`.

  Module: `Plutarch.Api.V1.Contexts`; `Plutarch.Api.V2.Contexts`

- Added `plistData` builtin function wrapper.

  Module: `Plutarch.Builtin`

- Added `PEq` superclass constraint to `POrd`

  Included by [#326](https://github.com/Plutonomicon/plutarch/pull/326)

- Added module `Plutarch.Show` with the `PShow` type class, as well as functions `pshow` and `ptraceShowId`.

  Started by [#352](https://github.com/Plutonomicon/plutarch/pull/352)

- Add `puncons` and `ptryUncons` functions for deconstructing list.

  Started by: [#333](https://github.com/Plutonomicon/plutarch/pull/333)

- Add generic deriving for `PEq`

  Started by [#335](https://github.com/Plutonomicon/plutarch/pull/335)
- `Plutarch.Prelude` and `Plutarch.List` now export pfind, pelemAt, preserve and pcheckSroted.

  Started by: [#306](https://github.com/Plutonomicon/plutarch/pull/306)

- Added module `Plutarch.FFI` for interoperability with PlutusTx.

- Added `DerivePConstantViaBuiltin`, deprecating `DerivePConstantViaNewtype`.

- `TermCont`: Parametrize by result type; add `MonadFail` instance; etc.

  Also, export from `Plutarch.TermCont`, and then from `Plutarch.Prelude` (TermCont is no longer exported by `Plutarch.Internal`).

  Started by: [#226](https://github.com/Plutonomicon/plutarch/pull/226)

- Add `PlutusType` generic deriving support for data encoded Plutarch types, via `PIsDataRepr` and `PIsDataReprInstances`.

  All existing ledger api types now have `PlutusType` instances - not just `PMatch`.

  Started by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

- Add `PDataRecord` construction utilities, necessary for full usage of data encoded `PlutusType` instances.

  In particular, you can build `PDataRecord`s with `pdcons` and `pdnil` - refer to the guide for more info.

  `pdcons` and `pdnil` are also exported from `Plutarch.Prelude`.

  Also add `PlutusType` instance for `PDataRecord`.

  Module: `Plutarch.DataRepr.Internal`

  Started by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

- Export `PLabeledType ((:=))` from `Plutarch.Prelude`.

  Added by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

- Add `pconstantData` - an efficient way of building data encoded constants directly.

  This is semantically equivalent to `pdata . pconstant` but does not do any extra builtin function call.

  Module: `Plutarch.Builtin`

  Added by: [#251](https://github.com/Plutonomicon/plutarch/pull/251)

- Added APIs for constructing, compiling, serialising & hashing Plutarch scripts.

  Type synonyms for Plutarch-typed scripts `PValidator`,`PMintingPolicy` & `PStakeValidator`.

  `mkValidator`, `mkStakeValidator` & `mkMintingPolicy` functions, for creating Plutus API compatible scripts.

  `validatorHash`, `mintingPolicySymbol` & `stakeValidatorHash` to obtain script hashes.

  Module: `Plutarch.V1.Api`

  See: `Plutarch.ScriptsSpec` in `plutarch-test` for usage examples.

  Added by: [#267](https://github.com/Plutonomicon/plutarch/pull/267)
- Add `PTuple` construction and related utilities.

  Module: `Plutarch.Api.V1.Tuple`

  Added by: [#255](https://github.com/Plutonomicon/plutarch/pull/255)

- Add `PIsData` instances for `PUnit` and `PBuiltinPair (PAsData a) (PAsData b)`.

  It's helpful to mentally note that `PTuple a b`, `PAsData (PTuple a b)` and `PBuiltinPair (PAsData a) (PAsData b)` all have the exact same underlying representation. See `Plutarch.Api.V1.Tuple` for no-op conversion functions.

  Module: `Plutarch.Builtin`

  Added by: [#255](https://github.com/Plutonomicon/plutarch/pull/255)

- Add implicit `pfromData` for `hrecField` and the record dot. Add implicit `pfromData` for `pfield`.

  Module: `Plutarch.DataRepr`

  Added by: [#235](https://github.com/Plutonomicon/plutarch/pull/270)

- Add `Plutarch.Test` for testing Plutarch code with goldens for UPLC printing and Plutus benchmarks.

- Add Conversion types `PTryFrom`, `PMaybeFrom` and `PFrom`

  Module: `Plutarch.TryFrom`

  Added by: [#326](https://github.com/Plutonomicon/plutarch/pull/326)

- `plutarch-extra`: Add a new directory scaffold "`plutarch-extra`" which will be home to everything too specific to not be in the
  main Plutarch repo. Also refactored the test library.

  Directory: `plutarch-extra`

  Added by: [#329](https://github.com/Plutonomicon/plutarch/pull/329)

- `plutarch-extra` export merged Prelude

  Module: `Plutarch.PPrelude`

  Added by: [#356](https://github.com/Plutonomicon/plutarch/pull/356)

- Add `PConstant` instance for `Maybe`, with corresponding `PLift` instance for `PMaybeData`.

  Added by: [#371](https://github.com/Plutonomicon/plutarch/pull/371)

- Add `POrd` and `PEq` derivation for data encoded types via `PIsDataReprInstances`.

  Added by: [#371](https://github.com/Plutonomicon/plutarch/pull/371)

- Make `PRational` construction machinery fail when the denominator is 0.

  Fixed by: [#299](https://github.com/Plutonomicon/plutarch/pull/299)

- Rename `PConstant` (the typeclass) to `PConstantDecl`. `PConstant` is now a type alias with extra constraints for better type checking.

  Add `PLiftData` and `PConstantData` type aliases.

  Added by: [#354](https://github.com/Plutonomicon/plutarch/pull/354)

- Remove `hrecField` export. Use `getField` instead.

  Removed by: [#415](https://github.com/Plutonomicon/plutarch/pull/415)

- Rename the `"data"` field of `PTxInfo` to `"datums"`.

  Renamed by: [#415](https://github.com/Plutonomicon/plutarch/pull/415)

- Add `Num` instance for `PPOSIXTime` and export its constructor.

  Added by: [#415](https://github.com/Plutonomicon/plutarch/pull/415)

- `PlutusType` is now a superclass of `PIsDataRepr`, strengthening the existing `PMatch` superclass constraint.

  Added by: [#415](https://github.com/Plutonomicon/plutarch/pull/415)

- Add `PlutusType` instance for `PDataSum`. `PDataSum` can now be hand-constructed.

  Added by: [#345](https://github.com/Plutonomicon/plutarch/pull/345)

- Add `HRecOf`, `PMemberFields`, and `PMemberField` utility types.

  Module: `Plutarch.DataRepr`.

  Added by: [#466](https://github.com/Plutonomicon/plutarch/pull/466)

- Move `Plutarch.ListUtils` to `Plutarch.Extra.List`.

  Added by: [#466](https://github.com/Plutonomicon/plutarch/pull/466)

- Add various `TermCont` utilities: `ptraceC`, `pletFieldsC`, `ptryFromC`, `pguardC`, and `pguardC'`.

  Module: `Plutarch.Extra.TermCont`.

  Added by: [#466](https://github.com/Plutonomicon/plutarch/pull/466)

# 1.1.0

- General repository changes.
  - The Plutarch repo has moved to GHC 9.2.1. Projects using GHC 8.10.7 should still be able to depend on Plutarch. There is CI in place to ensure compatibility.

    Relevant PR: [#86](https://github.com/Plutonomicon/plutarch/pull/86)

    CI added by: [#188](https://github.com/Plutonomicon/plutarch/pull/188)
  - Major nix updates.

    Started by: [#75](https://github.com/Plutonomicon/plutarch/pull/75)
  - Benchmarks and perf diffing on PRs to keep track of performance regressions.

    Worked on through:
    - [#102](https://github.com/Plutonomicon/plutarch/pull/102)
    - [#144](https://github.com/Plutonomicon/plutarch/pull/144)
    - [#146](https://github.com/Plutonomicon/plutarch/pull/146)
    - [#164](https://github.com/Plutonomicon/plutarch/pull/164)
    - [#167](https://github.com/Plutonomicon/plutarch/pull/167)
    - [#178](https://github.com/Plutonomicon/plutarch/pull/178)

- Significantly improve `plam` type inference - works seamlessly now (!!!)

  Worked on through:
  - [#29](https://github.com/Plutonomicon/plutarch/pull/29)
  - [#149](https://github.com/Plutonomicon/plutarch/pull/149)
  - [#162](https://github.com/Plutonomicon/plutarch/pull/162)
  - [#168](https://github.com/Plutonomicon/plutarch/pull/168)
  - [#170](https://github.com/Plutonomicon/plutarch/pull/170)

  Relevant issue: [#2](https://github.com/Plutonomicon/plutarch/issues/2)
- Many, many optimizations on generated code.

  Worked on through:
  - [#34](https://github.com/Plutonomicon/plutarch/pull/34)
  - [#37](https://github.com/Plutonomicon/plutarch/pull/37)
  - [#42](https://github.com/Plutonomicon/plutarch/pull/42)
  - [#44](https://github.com/Plutonomicon/plutarch/pull/44)
- Add haddocks and examples to many functions

  Started by: [#49](https://github.com/Plutonomicon/plutarch/pull/49)
- Add many new utility functions, instances, and builtin function synonyms.

  <details>
  <summary> Breakdown of additions </summary>

  - Boolean utilities:- `pnot`, `#&&`/`pand`/`pand'`, `#||`/`por`/`por'`

    Module: `Plutarch.Bool`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - ByteString utilities:- `pconsBS`, `psliceBS`, `plengthBS`, `pindexBS`

    Module: `Plutarch.ByteString`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - Cryptographic hashing utilities:- `psha2_256`, `psha3_256`, `pblake2b_256`, `pverifySignature`

    Module: `Plutarch.Crypto`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PEq`, `POrd`, `Semigroup`, and `Monoid` instances for `PUnit`

    Module: `Plutarch.Unit`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - UTF-8 encode/decode functions:- `pencodeUtf8`, `pdecodeUtf8`

    Module: `Plutarch.String`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PIntegral` typeclass

    Module: `Plutarch.Integer`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PIntegral` instance for `PInteger`

    Module: `Plutarch.Integer`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PEq` instance for `PData` and `PAsData`

    Module: `Plutarch.Builtin`

    Added in: [#38](https://github.com/Plutonomicon/plutarch/pull/38)
  - Tracing functions:- `ptrace`, `ptraceIfFalse`, `ptraceIfTrue`, `ptraceError`

    Module: `Plutarch.Trace`

    Added in: [#39](https://github.com/Plutonomicon/plutarch/pull/39)
  - Builtin pair construction utility:- `ppairDataBuiltin`

    Module: `Plutarch.Builtin`

    Added in: [#50](https://github.com/Plutonomicon/plutarch/pull/50)
  - *Loads* of awesome list utilities thanks to [#63](https://github.com/Plutonomicon/plutarch/pull/63)!

    Module: `Plutarch.List`
  - `PIsData` instance for `PBool`

    Module: `Plutarch.Builtin`

    Added in: [#110](https://github.com/Plutonomicon/plutarch/pull/110)

  </details>
- Add `PIsData` for conversion between normal builtin types and their `Data` representation.

  Initially added in: [#31](https://github.com/Plutonomicon/plutarch/pull/31)
- Add `PAsData` for preserving more type information regarding `Data` encoded values.

  Initially added in: [#31](https://github.com/Plutonomicon/plutarch/pull/31)
- Add `PDataRepr` and related machinery to ergonomically work with `Constr` encoded data.

  There are also **generic derivers** (!!) to implement the related typeclasses for custom data types.

  Module: `Plutarch.DataRepr`

  Worked on through
  - [#31](https://github.com/Plutonomicon/plutarch/pull/31)
  - [#169](https://github.com/Plutonomicon/plutarch/pull/169)
  - [#173](https://github.com/Plutonomicon/plutarch/pull/173)
  - [#176](https://github.com/Plutonomicon/plutarch/pull/176)
  - [#171](https://github.com/Plutonomicon/plutarch/pull/171)
  - [#185](https://github.com/Plutonomicon/plutarch/pull/185)
- Add `PLift` and `PConstant` - enabling conversion between Plutarch terms and Haskell types.

  This comes with convenient derivers. See the relevant section on the guide.

  Module: `Plutarch.Lift`; another deriver within `Plutarch.DataRepr.Internal`

  Worked on through
  - [#62](https://github.com/Plutonomicon/plutarch/pull/62)
  - [#109](https://github.com/Plutonomicon/plutarch/pull/109)
  - [#130](https://github.com/Plutonomicon/plutarch/pull/130)
- Deprecate `punsafeConstant`. Use `pconstant` instead!
- Add `PIsList` & `PList` + instances for `PBuiltinList` - ergonomic list functions for all!

  Module: `Plutarch.List`; another instance in `Plutarch.Builtin`

  Added in: [#63](https://github.com/Plutonomicon/plutarch/pull/63)
- Mutually recursive data types, scott encoded records and more provided by `Plutarch.Rec`!

  Module: `Plutarch.Rec`

  Worked on through:
  - [#60](https://github.com/Plutonomicon/plutarch/pull/60)
  - [#114](https://github.com/Plutonomicon/plutarch/pull/114)
  - [#125](https://github.com/Plutonomicon/plutarch/pull/125)
  - [#175](https://github.com/Plutonomicon/plutarch/pull/175)
- The Plutus V1 ledger api types implemented in Plutarch!

  Module: `Plutarch.Api.V1`

  Worked on through:
  - [#66](https://github.com/Plutonomicon/plutarch/pull/66)
  - [#130](https://github.com/Plutonomicon/plutarch/pull/130)
  - [#151](https://github.com/Plutonomicon/plutarch/pull/151)
  - [#161](https://github.com/Plutonomicon/plutarch/pull/161)
  - [#163](https://github.com/Plutonomicon/plutarch/pull/163)
  - [#173](https://github.com/Plutonomicon/plutarch/pull/173)
- Implement Plutarch rational type.

  Module: `Plutarch.Rational`

  Added in: [#89](https://github.com/Plutonomicon/plutarch/pull/89)
- The `s` in `Term s _` has a fixed kind now. `data S`, `s :: S`

  Module: `Plutarch.Internal`

  Added in: [#115](https://github.com/Plutonomicon/plutarch/pull/115/files)
- Add `PType`, a synonym to `S -> Type` - i.e the kind of Plutarch types. In particular, the kind of the 3rd type parameter of `Term`.

  Module: `Plutarch.Internal`

  Added in: [#115](https://github.com/Plutonomicon/plutarch/pull/115/files)
- Convenient do syntax with `QualifiedDo` (!!!)

  Module: `Plutarch.Monadic`

  Added in: [#119](https://github.com/Plutonomicon/plutarch/pull/119)
- Add `DerivePNewtype` to derive common typeclasses for Plutarch newtypes.

  Module: `Plutarch`; `Plutarch.Builtin`; `Plutarch.Bool`; `Plutarch.Integer`

  Added in: [#151](https://github.com/Plutonomicon/plutarch/pull/151)
- A whole lot more exports from `Plutarch.Prelude`

  Module: `Plutarch.Prelude`

  Added in: [#181](https://github.com/Plutonomicon/plutarch/pull/181)
- Move out `punsafe*` functions from `Plutarch` and into `Plutarch.Unsafe`.
- Add generic deriving for `PlutusType` with scott encoding representation.

  Added in: [#189](https://github.com/Plutonomicon/plutarch/pull/189)

Thanks to:

- @blamario
- @emiflake
- @Geometer1729
- @kozross
- @L-as
- @MatthewCroughan
- @sergesku
- @srid
- @t1lde
- @TotallyNotChase

# 1.0.0

Somewhat stable release
