{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module HW3.Pretty (prettyValue) where

import Data.Foldable (toList)
import Data.ByteString (ByteString, unpack)
import Data.Scientific
import qualified Data.Map as MP
import GHC.Real (Ratio ((:%)))
import Prettyprinter
import Prettyprinter.Render.Terminal
import HW3.Base
import Numeric

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  -- T1
  (HiValueNumber n) -> prettyNumber n
  (HiValueFunction f) -> pretty $ hiFunName f
  -- T2
  (HiValueBool b) -> if b then "true" else "false"
  -- T4
  HiValueNull -> "null"
  (HiValueString s) -> viaShow s
  -- T5
  (HiValueList l) -> case l of
    [] -> "[ ]"
    _ -> encloseSep "[ " " ]" ", " (prettyValue <$> (toList l))
  -- T6
  (HiValueBytes b) -> prettyBytes b
  -- T8
  (HiValueTime t) -> "parse-time(\"" <> viaShow t <> "\")"
  -- T9-T10
  (HiValueAction ac) -> prettyAction ac
  -- T11
  (HiValueDict m) -> prettyDict m

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  -- T7
  HiActionCwd -> "cwd"
  (HiActionRead fp) -> surround (viaShow fp) "read(" ")"
  (HiActionWrite fp s) -> surround (viaShow fp <> ", " <> viaShow s) "write(" ")"
  (HiActionMkDir fp) -> surround (viaShow fp) "mkdir(" ")"
  (HiActionChDir fp) -> surround (viaShow fp) "cd(" ")"
  -- T8
  HiActionNow -> "now"
  -- T9
  (HiActionRand x y) -> surround (viaShow x <> ", " <> viaShow y) "rand(" ")"
  -- T10
  (HiActionEcho s) -> surround (viaShow s) "echo(" ")"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber n@(p :% q) = case (p, q) of
  (x, 1) -> pretty x
  (x, y) -> case fromRationalRepetendUnlimited n of
    (s, Nothing) -> pretty (formatScientific Fixed Nothing s)
    _ -> prettyFraction (quotRem x y) y
  where
    prettyFraction (0, r) d = pretty r <> slash <> pretty d
    prettyFraction (q', r) d = pretty q' <+> pretty (if r < 0 then '-' else '+') <+> prettyFraction (0, abs r) d

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes = \case
  "" -> "[# #]"
  bs -> surround (fillSep (map (pretty . adder . flip showHex "") $ unpack bs)) "[# " " #]"
  where
    adder a = if length a < 2 then '0' : a else a

prettyDict :: MP.Map HiValue HiValue -> Doc AnsiStyle
prettyDict = \case
  m | null m -> "{ }"
  m -> encloseSep "{ " " }" ", " (prettyPairs <$> MP.assocs m)
  where
    prettyPairs = \case (k,v) -> prettyValue k <> ":" <+> prettyValue v