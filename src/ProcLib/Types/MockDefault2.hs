{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- !!! REMOVE THIS WHEN INTEGRATING WITH ProcLib.Types.MockDefault !!!
{-# LANGUAGE IncoherentInstances #-}

module ProcLib.Types.MockDefault2
  ( MockDefault( mockDef ) )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Types.MockDefault  ( MockDefault( mockDef ) )

--------------------------------------------------------------------------------

-- !!! REMOVE IncoherentInstances WHEN INTEGRATING WITH MockDefault !!!

instance MockDefault [α] where
  mockDef = []

instance (MockDefault α, MockDefault β) ⇒ MockDefault (α,β) where
  mockDef = (mockDef,mockDef)

instance (MockDefault α,MockDefault β,MockDefault γ) ⇒ MockDefault (α,β,γ) where
  mockDef = (mockDef,mockDef,mockDef)

-- !!! REMOVE IncoherentInstances WHEN INTEGRATING WITH MockDefault !!!

-- that's all, folks! ----------------------------------------------------------
