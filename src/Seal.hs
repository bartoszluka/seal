{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Seal (
    eval,
    parseFile,
    EvalError (..),
) where

import Seal.Interpreter (EvalError (..), eval)
import Seal.Parser (parseFile)
