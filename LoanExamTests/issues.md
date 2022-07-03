## ```testInfo/memoryRepr/typeRepr``` types
I update testInfo.memory.types before serializing test information to files.
While strings are OK-ish in terms of the ability to obtain type information, it still concerns me that
```Type -> String```, ```String -> Type``` have to happen twice.
Any objections against changing the structure of the source code in a way that strings appear only on app boundaries:
just before and right after serialization/deserialization?