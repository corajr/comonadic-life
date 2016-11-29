module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.FS (FS)
import Node.Process (PROCESS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Jack.Runner (jackMain)

-- main :: Eff (fs :: FS, process :: PROCESS, console :: CONSOLE) Unit
main = do
  -- jackMain [
  --   "Test.LifeComonadLaws"
  -- ]
  discover "Test\\..*Spec" >>= run [consoleReporter]



