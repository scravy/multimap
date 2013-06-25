##1.3
  - Reorganized the modules structure and renamed data-types accordingly.
  
    ###Data.SetMultiMap:
      - Changed the signature of `delete`, added `deleteAll`.
      - Ditched the internal counters, making the size operation `O(n)`, but increasing performance of most modification operations. 
      - Ditched the redundant `numKeys` and `numValues`. Introduced `keysSize` with an `Int`-type.
      - Reversed the order of arguments of `member` and `notMember` functions to accomodate the standards.
      - `elems` now returns a flattened list of all elements.
      - Switch to a strict version of underlying map.
      - Added `alterF`
      - Added `replace`

    ###Data.BagMultiMap:
      - Changed the signature of `delete`, added `deleteAll`.

##1.2.1
  - Fixed typos in the documentation.

##1.2
  - Added `Data.SetMap`, renamed `Multimap` to `Data.MultiMap`.
  - Fixed the type of `delete`. Derive instances for `Data` and `Typeable`.

##1.1
  - `!` had its arguments flipped. Fixed.
  - Also added `fromMap`.
