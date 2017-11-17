module Simple where


import           CMark
import           CMark.Macro


fooBar :: Node -> Node
fooBar x =
  runMacro $
    replaceString "foo" "bar" x

fooBarFix :: Node -> Node
fooBarFix x =
  runMacro $
    fixpoint x $
      replaceString "foobar" "bar"
