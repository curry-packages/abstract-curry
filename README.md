abstract-curry
==============

This package contains libraries to deal with AbstractCurry programs.
AbstractCurry is a representation of Curry programs oriented
towards the source code of Curry. Thus, it can be used
to generate readable Curry programs, e.g., by the Web framework Spicey.

The package contains the following modules:

* `AbstractCurry.Build`: This module provides some useful operations
  to write programs that generate AbstractCurry programs
  in a more compact and readable way.
* `AbstractCurry.Files`: This module defines operations to read and write
  AbstractCurry programs.
* `AbstractCurry.Pretty`: This module provides a pretty-printer
  for AbstractCurry modules.
* `AbstractCurry.Select`: This module provides some useful operations
  to select components in AbstractCurry programs, i.e.,
  it provides a collection of selector functions for AbstractCurry.
* `AbstractCurry.Show`: This module provides
  transformation and update operations on AbstractCurry programs.
  Since the transformations are defined recursively on structured types,
  they are useful to construct specific transformations on AbstractCurry
  programs. In particular, this library contains the transformation
  `renameCurryModule` to rename an AbstractCurry module.
* `AbstractCurry.Types`: This module defines the data types to represent
  AbstractCurry programs in Curry.
