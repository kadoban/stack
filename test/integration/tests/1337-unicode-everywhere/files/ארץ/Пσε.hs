{-# LANGUAGE CPP #-}
module Пσε
    ( θυπε
    ) where

θυπε :: String
#ifdef BREAK_THINGS
θυπε = "以呂波耳本部止" ++ otherThing
#else
θυπε = "以呂波耳本部止"
#endif

#ifdef FIX_THINGS
otherThing :: String
otherThing = "stuff"
#endif
