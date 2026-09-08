# stablehlo (development version)

## Breaking changes

* A `Shape` *is* its integer vector now, with a class attached, rather than a
  list wrapping one. `length(shape)` is the rank, `shape[i]` is an axis size,
  and `shape$dims` is gone -- read the sizes with `unclass()`.

* `==` and `!=` on a `Shape` now raise an error rather than answering. Once an
  axis size can be `NA`, "are these two shapes equal" is three questions that
  disagree exactly where it matters, and an operator cannot say which was
  meant; the error names them and points at the helper for each.

* `shape()` has no `Shape` method any more, for the same reason as the first
  point: a `Shape` already *is* its integer vector, so there is nothing to
  extract. `shape()` keeps working on a `ValueType`, `TensorType` or
  `Constant`.

## Features

* Shape inference understands axis sizes that are only known at run time. A
  constraint over an `NA` axis size is refused only when it is *certainly*
  violated and left to the runtime otherwise, and where one operand knows a
  size the other does not, the known size wins: `add(tensor<?xf32>,
  tensor<3xf32>)` used to be an error and now infers `tensor<3xf32>`.

  Every op that can take a dynamic operand now does. Of the 85 that can be
  driven with one, 78 carry a `?` through; the rest have a result whose
  extents come from an attribute rather than an operand
  (`broadcast_in_dim`, `reshape`, `slice`, `dynamic_slice`,
  `get_dimension_size`, and the offset axes of `gather` and the second result
  of `rng_bit_generator`), and they no longer refuse an operand whose size
  they cannot check. `transpose`, `reverse`,
  `convert` and `pad` needed no change -- they only index axes or do
  arithmetic that `NA` already propagates through correctly. `reduce_window`'s
  arithmetic likewise, but its "all inputs share a shape" check had to be
  folded like `reduce`'s.

* Control flow -- `if`, `case`, `while` -- keeps requiring its branch and
  carried types to match *exactly*, and now says so deliberately rather than
  by omission. Widening a branch that knows an axis size against one that does
  not is tempting, since only one branch runs; SPEC forbids it (`if` (C2),
  `case` (C3), `while` (C2) are all equalities) and IREE cannot lower the
  widened form at all, because it maps these to `scf.if`/`scf.while`, whose
  yielded type must match the region's declared type. A program that needs the
  widening has to do it explicitly, inside the branch.

* The dynamic-op family is complete: every op SPEC.md documents as taking its
  sizes as *operands* rather than attributes is now available --
  `hlo_dynamic_broadcast_in_dim()`, `hlo_dynamic_iota()`,
  `hlo_dynamic_reshape()`, `hlo_dynamic_pad()`, `hlo_dynamic_gather()`,
  `hlo_dynamic_conv()`, alongside the `hlo_dynamic_slice()` and
  `hlo_dynamic_update_slice()` that were already here, plus
  `hlo_get_dimension_size()` for reading an axis size as a value. Each op
  whose *result* extents are data takes a `shape` argument giving the result's
  static shape with `NA` where a size is only known at run time, since such a
  result cannot be inferred; it is a claim rather than a check.

  `hlo_dynamic_gather()` and `hlo_dynamic_conv()` share their static
  counterpart's inference, run with the moved operand marked unknown: every
  check that does not depend on it still fires, the ones that do defer, and
  the result comes back with `?` exactly on the axes that operand determines.

  A caveat on the whole family: most of these ops cannot be lowered by a
  backend directly. `iree-compile` refuses all but `dynamic_reshape` and
  `real_dynamic_slice` when their size operands are not constants, and XLA
  refuses a `?` entry point outright. The supported route is to refine the
  program back to concrete shapes with `pjrt::pjrt_refine_shapes()`, which
  folds the dynamic op away; that is what the tests do, and it works for every
  op in the family.

* Added `hlo_real_dynamic_slice()`, which is in the StableHLO dialect but not
  in SPEC.md. It is the only op that gives a result extent computed from the
  *data* rather than from a shape, which is what a program needs to return
  just the live part of a buffer -- the distinct elements of a vector, the rows
  passing a filter. Note that XLA cannot compile it at all ("can't be
  translated to XLA HLO"), and shape refinement cannot remove it, since no
  shape in the program determines the extent; it needs a backend that compiles
  dynamic shapes natively.

* Dynamic programs are checked two ways, because each catches what the other
  cannot. One compiles the dynamic program directly and runs it at several
  sizes, which needs a backend that accepts a `?` entry point. The other
  refines it back to concrete shapes with `pjrt::pjrt_refine_shapes()` and
  runs that, which additionally checks that the result type the refinement
  pass derives is the one this package's inference derives from the static
  shapes -- two independent derivations that must not disagree.

## Bug fixes

* `hlo_triangular_solve()` now rejects operands that are not of floating-point
  type, as required by the StableHLO spec.

# stablehlo 0.4.0

## Features

* Added `hlo_convolution()`
* `hlo_dot_general()` gained a `precision_config` argument, which is
  either `NULL` or one or two of `"DEFAULT"`, `"HIGH"` and `"HIGHEST"`.

## Breaking changes

* Adopted tengen's enum-style `DataType`. The `BooleanType()`, `FloatType()`,
  `IntegerType()` and `UIntegerType()` constructors were removed, use
  `dtype()` / `as_dtype()` instead.
* The package now requires R >= 4.4.0.

## Performance

* The package was optimized w.r.t. runtime performance.
  This was achieved by reducing the number of classes
  that are used internally when creating `Func`s.
  The `hlo_<*>` user API remains unaffected.
* The `hlo_*` builders of common ops gained an `output_types` argument.
  When the output types are known ahead of time (e.g. from a lowering that
  ran type inference at trace time), passing them skips redundant inference
  and its input validation.

## Bug fixes

* emit width-correct hex for f64 NaN/Inf constants
* `hlo_reduce_window()` now validates the shape of `padding` against its
  declared type instead of its R representation.


# stablehlo 0.3.0

## Features

* Added support for CHLO ops, a higher-level companion dialect to stableHLO
  that is lowered to stableHLO during compilation. New ops:
  * Inverse trig: `hlo_acos()`, `hlo_asin()`, `hlo_atan()`.
  * Hyperbolic: `hlo_cosh()`, `hlo_sinh()`, `hlo_acosh()`, `hlo_asinh()`,
    `hlo_atanh()`.
  * Gamma family: `hlo_lgamma()`, `hlo_digamma()`, `hlo_polygamma()`.
  * Error / Bessel / misc: `hlo_erf()`, `hlo_erfc()`, `hlo_erf_inv()`,
    `hlo_bessel_i1e()`, `hlo_square()`.
  * Float predicates: `hlo_is_inf()`, `hlo_is_pos_inf()`, `hlo_is_neg_inf()`.
  * Selection: `hlo_top_k()` returning the top-k values and their indices
    along the last dimension.
* `OpName()` and `new_Op()` gain a `dialect` argument (default `"stablehlo"`)
  to support ops from other MLIR dialects.

## Bug fixes

* `hlo_reduce_window()` now works on rank-1 inputs.

# stablehlo 0.2.0

## Features

* Added support for Modules

## Bug fixes

* Constants +-Inf/NaN of dtype f64 are now correctly created.
* Fixed assembly format for `select` op with all boolean (i1) types.

## Miscellaneous

* Use a simpler StableHLO string format for improved readability

# stablehlo 0.1.0

* Initial release
