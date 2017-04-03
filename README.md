# eta-ffi
```
eta-ffi -jar <jar name here>.jar <package name here>
Flags:
-classplath (same format as javac)
-ffi (accepts .ffi files which contain a 
  - mapping from Java class/interface/enum to an Eta type, Eta module, and Eta package
-target (which Eta version to target)
-package-prefix (package prefix eg: Spark)
--global-single-file (Default) Means all methods for a given package should be in a single file called Methods.hs
--single-file [package]
--multi-file [package]
---global-multi-file (Manual) Means individual Eta module per Java class
[input-package] support glob patterns (ex: java.util.*) (ex: java.util.**)
Single * means just that package
** means nested packages as well.
```
