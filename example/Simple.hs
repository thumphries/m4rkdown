module Simple where


import           CMark
import           CMark.Macro


fooBar :: Node -> Node
fooBar x =
  runMacro $
    replaceString "foo" "bar" x

fooBarFix :: Node -> Node
fooBarFix x =
  runMacro $ do
    y <- fixpoint x $ replaceString "foobar" "bar"
    replaceString "bar" "baz" y
