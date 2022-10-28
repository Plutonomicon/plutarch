{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty (prettyTerm, prettyTerm', prettyScript) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), modify, modify')
import Data.Foldable (fold)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Traversable (for)

import System.Random.Stateful (mkStdGen, newSTGenM)

import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Plutarch.Internal (ClosedTerm, Config, compile)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1.Scripts (Script (unScript))
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  DefaultUni,
  Program (_progTerm),
  Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var),
 )

import Plutarch.Pretty.Internal.BuiltinConstant (prettyConstant)
import Plutarch.Pretty.Internal.Config (indentWidth)
import Plutarch.Pretty.Internal.Name (freshVarName, smartName)
import Plutarch.Pretty.Internal.TermUtils (
  unwrapApply,
  unwrapBindings,
  unwrapLamAbs,
  pattern IfThenElseLikeAST,
 )
import Plutarch.Pretty.Internal.Types (
  PrettyCursor (Normal, Special),
  PrettyMonad,
  PrettyState (PrettyState, ps'cursor, ps'nameMap),
  builtinFunAtRef,
  forkState,
  insertBindings,
  insertName,
  nameOfRef,
  normalizeCursor,
  specializeCursor,
 )

-- | 'prettyTerm' for pre-compiled 'Script's.
prettyScript :: Script -> PP.Doc ()
prettyScript = prettyUPLC . _progTerm . unScript

{- | Prettify a Plutarch term.

This will call 'error' if there's a compilation failure. Use 'prettyTerm'' for a non-partial version.

== Example ==

@
import Plutarch.Prelude
import Plutarch.Api.V1
import Plutarch.Extra.TermCont

checkSignatoryPPlutus' s => Term s (PPubKeyHash #-> PScriptContext #-> PUnit)
checkSignatory = plam $ \ph ctx' -> unTermCont $ do
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
  purph <- pmatchC ctx.purpose
  pure $ case purph of
    PSpending _ ->
      let signatories = pfield @"signatories" # ctx.txInfo
      in pif
          (pelem # pdata ph # pfromData signatories)
          -- Success!
          (pconstant ())
          -- Signature not present.
          perror
    _ -> ptraceError "checkSignatoryCont: not a spending tx"
@

Prettification result:

@
let frSndPair = !!sndPair
    unDataSum = (\xF -> frSndPair (unConstrData xF))
    frTailList = !tailList
    frHeadList = !headList
    frIfThenElse = !ifThenElse
in (\oP4ECBT qsrxlF0Y7 ->
      let cjlB6yrGk = unDataSum qsrxlF0Y7
          cRFO = unConstrData (frHeadList (frTailList cjlB6yrGk))
          cs9iR = !!fstPair cRFO
          w4 = frSndPair cRFO
      in if equalsInteger 1 cs9iR
           then if (\vModHwqYB ->
                      let blM6d67 =
                            (\x5sad ePDSInSEC ->
                               !(!!chooseList
                                   ePDSInSEC
                                   ~False
                                   ~(if equalsData
                                          (frHeadList ePDSInSEC)
                                          vModHwqYB
                                       then True
                                       else x5sad (frTailList ePDSInSEC))))
                          mC = (\jfZs -> blM6d67 (\itzT -> jfZs jfZs itzT))
                      in blM6d67 (\ispwp_oeT -> mC mC ispwp_oeT))
                     (bData oP4ECBT)
                     (unListData
                        let q6X3 = frHeadList cjlB6yrGk
                        in frHeadList
                             let olbZ = unDataSum q6X3
                             in frTailList
                                  (frTailList
                                     (frTailList
                                        (frTailList
                                           (frTailList
                                              (frTailList
                                                 (frTailList olbZ)))))))
                  then ()
                  else ERROR
           else !(!trace "checkSignatoryCont: not a spending tx" ~ERROR))
@

== Semantics ==

=== Constants ===

- Builtin integers are printed as regular integers. [0-9]+
- Builtin bytestrings are printed in hex notation, prefixed by `0x`. 0x[0-9a-f]+/i
- Builtin strings are printed as is.
- Builtin unit is printed as the unit literal. ()
- Builtin booleans are printed as the literal `True` or `False`.
- Builtin lists are prettified as list literals, i.e delimited with `[` and `]`.
- Builtin pairs are prettified as 2-ary tuple literals, e.g. `(a, b)`.
- `I` data (i.e data encoded integers) are prettified like builtin integers with a `#` prefix. #[0-9]+
- `B` data (i.e data encoded bytestrings) are prettified like builtin bytestrings with a `#` prefix. #0x[0-9a-f]+/i
- `List` data (i.e data encoded lists) are prettified like builtin lists with a `#` prefix.
- `Map` data is printed like record literals. Delimited by `{` and `}`.

  Each key value pair is prettified like <key> = <value> and multiple pairs are joined with `,`.

  For example, `Map [(I 42, I 0), (I 100, I 1)]` is prettified as `{ #42 = #0, #100 = #1 }`
- Constr data has two core elements in its prettified form:

  - The constructor index, prettified as an integer prefixed with `Σ` (sigma).
  - Its fields, prettified as a list.

  These two elements are then joined with a `.` (period).

  For example, `Constr 1 [I 42]` is prettified as "Σ1.[#42]".

=== Builtin functions ===

Builtin functions are prettified into their name, in title case.

=== Forced term ===

Forced terms are prefixed with a `!`. The unary operator `!` has higher fixity than function application.

=== Delayed term ===

Delayed terms are prefixed with a `~`. The unary operator `~` has higher fixity than function application.

=== Var ===

Random names are generated for all variable bindings, and these names are used to refer to them.

Names are always unique, between 1 and 8 characters in length, and begin with a lowercase letter.

Names may consist of alphanumeric characters, underscore, or single quotes.

=== LamAbs ===

Lambdas are prettified similar to haskell lambdas, i.e `\x -> ...`.

Lambdas with multiple arguments are detected and simplified: `\x y z -> ...`.

=== Apply ===

Application is, simply, a space - just like haskell. `f x`.

Multi arg applications to the same function are detected and simplified: `f x y`.

=== Error term ===

`perror` is represented by the literal `ERROR`.

=== Special handling ===

To achieve better prettification, certain AST structures are given special handling logic.

- The AST structure produced by `plet` (Single `Apply` + `LamAbs` pair) is prettified into Haskell-like let bindings.
- Lazy if/then/else (`pif` in particular, not `pif'`) is detected and prettified into Haskell-like syntax:
  `if cond then expr1 else expr2`.

  Chains of if/then/else are nested:

  @
  if cond
    then expr1
    else if cond
      then expr2
      else expr3
  @
- When generating names for bindings, well known structures are identified and given special names.

  This machinery is made to be extensible in the future.

  For example, the structure of the `pfix` function is well known and constant - so it is simply called `fix` in the output.

  Bindings to forced builtin functions inherit the builtin function name, prefixed with a `fr`.
-}
prettyTerm :: Config -> ClosedTerm a -> PP.Doc ()
prettyTerm conf x = either (error . Txt.unpack) id $ prettyTerm' conf x

-- | Non-partial 'prettyTerm'.
prettyTerm' :: Config -> ClosedTerm p -> Either Text (PP.Doc ())
prettyTerm' conf x = prettyScript <$> compile conf x

{- This isn't suitable for pretty printing UPLC from any source. It's primarily suited for Plutarch output.
Practically speaking though, it should work with any _idiomatic_ UPLC.
-}
prettyUPLC :: Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
prettyUPLC uplc = runST $ do
  stGen <- newSTGenM $ mkStdGen 42
  (doc, _) <- runReaderT (go uplc) stGen `runStateT` PrettyState mempty mempty Normal
  pure doc
  where
    go :: Term DeBruijn DefaultUni DefaultFun () -> PrettyMonad s (PP.Doc ())
    go (Constant _ c) = pure $ prettyConstant c
    go (Builtin _ b) = pure $ PP.pretty b
    go (Error _) = pure "ERROR"
    go (Var _ (DeBruijn x)) = do
      PrettyState {ps'nameMap} <- get
      pure $ case nameOfRef x ps'nameMap of
        Just nm -> PP.pretty nm
        Nothing -> error "impossible: free variable"
    go (IfThenElseLikeAST (Force () (Builtin () PLC.IfThenElse)) cond trueBranch falseBranch) = do
      prettyIfThenElse (forkState . go) cond trueBranch falseBranch
    go ast@(IfThenElseLikeAST scrutinee cond trueBranch falseBranch) = do
      PrettyState {ps'nameMap} <- get
      case scrutinee of
        Var () (DeBruijn (builtinFunAtRef ps'nameMap -> Just PLC.IfThenElse)) ->
          prettyIfThenElse (forkState . go) cond trueBranch falseBranch
        _ -> case ast of
          Force _ t@Apply {} -> modify specializeCursor *> go t <&> ("!" <>)
          _ -> error "impossible: IfThenElseLikeAST"
    go (Force _ t) = modify specializeCursor *> go t <&> ("!" <>)
    go (Delay _ t) = modify specializeCursor *> go t <&> ("~" <>)
    go (LamAbs _ _ t') = do
      currState@PrettyState {ps'cursor} <- get
      let (depth, bodyTerm) = unwrapLamAbs 0 t'
      names <- traverse (const freshVarName) [0 .. depth]
      -- Add all the new names to the nameMap, starting with 0 index.
      put $ insertBindings names currState
      modify' normalizeCursor
      funcBody <- forkState $ go bodyTerm
      pure . parensOnCursor ps'cursor . PP.hang indentWidth $
        PP.sep
          [ "\\" <> PP.hsep (reverse $ map PP.pretty names) <+> "->"
          , funcBody
          ]
    go (Apply _ (LamAbs _ _ t) firstArg) = do
      PrettyState {ps'cursor} <- get
      let (restArgs, coreF) = unwrapBindings [] t
          helper (name, expr) = do
            modify' normalizeCursor
            valueDoc <- forkState $ go expr
            pure . PP.hang indentWidth $
              PP.sep
                [ PP.pretty name <+> "="
                , valueDoc
                ]
      firstName <- smartName firstArg
      firstBindingDoc <- helper (firstName, firstArg)
      modify' $ insertName firstName
      restBindingDoc <- fmap fold . for (reverse restArgs) $ \argExpr -> do
        newName <- smartName argExpr
        bindingDoc <- helper (newName, argExpr)
        modify' (insertName newName) $> PP.flatAlt PP.hardline "; " <> bindingDoc
      modify' normalizeCursor
      coreExprDoc <- go coreF
      pure . parensOnCursor ps'cursor $
        PP.align $
          PP.vsep
            [ "let" <+> PP.align (firstBindingDoc <> restBindingDoc)
            , "in" <+> coreExprDoc
            ]
    go (Apply _ t arg) = do
      PrettyState {ps'cursor} <- get
      let (l, f) = unwrapApply [] t
          args = l <> [arg]
      functionDoc <- forkState $ modify' specializeCursor *> go f
      argsDoc <- modify' specializeCursor *> traverse (forkState . go) args
      pure . parensOnCursor ps'cursor $
        PP.hang indentWidth $ PP.sep $ functionDoc : argsDoc

prettyIfThenElse ::
  (t -> PrettyMonad s (PP.Doc ann)) ->
  t ->
  t ->
  t ->
  PrettyMonad s (PP.Doc ann)
prettyIfThenElse cont cond trueBranch falseBranch = do
  PrettyState {ps'cursor} <- get
  modify' normalizeCursor
  condAst <- cont cond
  trueAst <- cont trueBranch
  falseAst <- cont falseBranch
  pure . parensOnCursor ps'cursor $
    PP.hang indentWidth $ PP.vsep ["if" <+> condAst, "then" <+> trueAst, "else" <+> falseAst]

-- | Wrap prettification result parens depending on cursor state.
parensOnCursor :: PrettyCursor -> PP.Doc ann -> PP.Doc ann
parensOnCursor cursor = do
  if cursor == Special then PP.parens else id
