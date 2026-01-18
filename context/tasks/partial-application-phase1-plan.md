# Partial Application Implementation - Phase 1: Constructor Migration


## Module Inventory

### Haskell Modules & Files

#### Bool Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Bool.hs` | - | - |
| `Glue.Lib.Bool.Eq.hs` | `Glue.Lib.Bool.EqSpec.hs` | eq |
| `Glue.Lib.Bool.Ne.hs` | `Glue.Lib.Bool.NeSpec.hs` | ne |
| `Glue.Lib.Bool.Lt.hs` | `Glue.Lib.Bool.LtSpec.hs` | lt |
| `Glue.Lib.Bool.Le.hs` | `Glue.Lib.Bool.LeSpec.hs` | le |
| `Glue.Lib.Bool.Gt.hs` | `Glue.Lib.Bool.GtSpec.hs` | gt |
| `Glue.Lib.Bool.Ge.hs` | `Glue.Lib.Bool.GeSpec.hs` | ge |
| `Glue.Lib.Bool.Not.hs` | `Glue.Lib.Bool.NotSpec.hs` | not |
| `Glue.Lib.Bool.If.hs` | `Glue.Lib.Bool.IfSpec.hs` | if |
| `Glue.Lib.Bool.When.hs` | `Glue.Lib.Bool.WhenSpec.hs` | when |
| `Glue.Lib.Bool.While.hs` | `Glue.Lib.Bool.WhileSpec.hs` | while |
| `Glue.Lib.Bool.Until.hs` | `Glue.Lib.Bool.UntilSpec.hs` | until |

#### IO Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.IO.hs` | - | - |
| `Glue.Lib.IO.Print.hs` | `Glue.Lib.IO.PrintSpec.hs` | print, println |
| `Glue.Lib.IO.Read.hs` | - | read-line |

#### Builtin Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Builtin.hs` | - | - |
| `Glue.Lib.Builtin.Def.hs` | `Glue.Lib.Builtin.DefSpec.hs` | def |
| `Glue.Lib.Builtin.Set.hs` | `Glue.Lib.Builtin.SetSpec.hs` | set |
| `Glue.Lib.Builtin.Lambda.hs` | `Glue.Lib.Builtin.LambdaSpec.hs` | lambda |
| `Glue.Lib.Builtin.Let.hs` | - | let |
| `Glue.Lib.Builtin.Import.hs` | `Glue.Lib.Builtin.ImportSpec.hs` | import |
| `Glue.Lib.Builtin.Try.hs` | `Glue.Lib.Builtin.TrySpec.hs` | try |
| `Glue.Lib.Builtin.Error.hs` | - | error |

#### List Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.List.hs` | - | - |
| `Glue.Lib.List.Append.hs` | `Glue.Lib.List.AppendSpec.hs` | append |
| `Glue.Lib.List.Butlast.hs` | `Glue.Lib.List.ButlastSpec.hs` | butlast |
| `Glue.Lib.List.Car.hs` | `Glue.Lib.List.CarSpec.hs` | car |
| `Glue.Lib.List.Cdr.hs` | `Glue.Lib.List.CdrSpec.hs` | cdr |
| `Glue.Lib.List.Cons.hs` | `Glue.Lib.List.ConsSpec.hs` | cons |
| `Glue.Lib.List.Drop.hs` | `Glue.Lib.List.DropSpec.hs` | drop |
| `Glue.Lib.List.Filter.hs` | `Glue.Lib.List.FilterSpec.hs` | filter |
| `Glue.Lib.List.Find.hs` | `Glue.Lib.List.FindSpec.hs` | find |
| `Glue.Lib.List.Flatten.hs` | `Glue.Lib.List.FlattenSpec.hs` | flatten |
| `Glue.Lib.List.Last.hs` | `Glue.Lib.List.LastSpec.hs` | last |
| `Glue.Lib.List.Length.hs` | `Glue.Lib.List.LengthSpec.hs` | length |
| `Glue.Lib.List.Map.hs` | `Glue.Lib.List.MapSpec.hs` | map |
| `Glue.Lib.List.Member.hs` | `Glue.Lib.List.MemberSpec.hs` | member |
| `Glue.Lib.List.Nth.hs` | `Glue.Lib.List.NthSpec.hs` | nth |
| `Glue.Lib.List.Partition.hs` | `Glue.Lib.List.PartitionSpec.hs` | partition |
| `Glue.Lib.List.Position.hs` | `Glue.Lib.List.PositionSpec.hs` | position |
| `Glue.Lib.List.Remove.hs` | `Glue.Lib.List.RemoveSpec.hs` | remove |
| `Glue.Lib.List.Reverse.hs` | `Glue.Lib.List.ReverseSpec.hs` | reverse |
| `Glue.Lib.List.Sort.hs` | `Glue.Lib.List.SortSpec.hs` | sort |
| `Glue.Lib.List.Take.hs` | `Glue.Lib.List.TakeSpec.hs` | take |
| `Glue.Lib.List.Zip.hs` | `Glue.Lib.List.ZipSpec.hs` | zip |

#### Math Module - Arithmetic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Math.Arithmetic.Add.hs` | `Glue.Lib.Math.Arithmetic.AddSpec.hs` | add |
| `Glue.Lib.Math.Arithmetic.Sub.hs` | `Glue.Lib.Math.Arithmetic.SubSpec.hs` | sub |
| `Glue.Lib.Math.Arithmetic.Mul.hs` | `Glue.Lib.Math.Arithmetic.MulSpec.hs` | mul |
| `Glue.Lib.Math.Arithmetic.Div.hs` | `Glue.Lib.Math.Arithmetic.DivSpec.hs` | div |
| `Glue.Lib.Math.Arithmetic.Mod.hs` | `Glue.Lib.Math.Arithmetic.ModSpec.hs` | mod |

#### Math Module - Power Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Math.Power.Exp.hs` | `Glue.Lib.Math.Power.ExpSpec.hs` | exp |
| `Glue.Lib.Math.Power.Pow.hs` | `Glue.Lib.Math.Power.PowSpec.hs` | pow |
| `Glue.Lib.Math.Power.Sqrt.hs` | `Glue.Lib.Math.Power.SqrtSpec.hs` | sqrt |

#### Math Module - Trigonometric Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Math.Trigonometric.Sin.hs` | `Glue.Lib.Math.Trigonometric.SinSpec.hs` | sin |
| `Glue.Lib.Math.Trigonometric.Cos.hs` | `Glue.Lib.Math.Trigonometric.CosSpec.hs` | cos |
| `Glue.Lib.Math.Trigonometric.Tan.hs` | `Glue.Lib.Math.Trigonometric.TanSpec.hs` | tan |
| `Glue.Lib.Math.Trigonometric.Asin.hs` | `Glue.Lib.Math.Trigonometric.AsinSpec.hs` | asin |
| `Glue.Lib.Math.Trigonometric.Acos.hs` | `Glue.Lib.Math.Trigonometric.AcosSpec.hs` | acos |
| `Glue.Lib.Math.Trigonometric.Atan.hs` | `Glue.Lib.Math.Trigonometric.AtanSpec.hs` | atan |

#### Math Module - Logarithmic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Math.Logarithmic.Log.hs` | `Glue.Lib.Math.Logarithmic.LogSpec.hs` | log |
| `Glue.Lib.Math.Logarithmic.Ln.hs` | `Glue.Lib.Math.Logarithmic.LnSpec.hs` | ln |
| `Glue.Lib.Math.Logarithmic.Lg.hs` | `Glue.Lib.Math.Logarithmic.LgSpec.hs` | lg |

#### Math Module - Utility Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `Glue.Lib.Math.Utility.Abs.hs` | `Glue.Lib.Math.Utility.AbsSpec.hs` | abs |
| `Glue.Lib.Math.Utility.Ceil.hs` | `Glue.Lib.Math.Utility.CeilSpec.hs` | ceil |
| `Glue.Lib.Math.Utility.Floor.hs` | `Glue.Lib.Math.Utility.FloorSpec.hs` | floor |
| `Glue.Lib.Math.Utility.Round.hs` | `Glue.Lib.Math.Utility.RoundSpec.hs` | round |
| `Glue.Lib.Math.Utility.Trunc.hs` | `Glue.Lib.Math.Utility.TruncSpec.hs` | trunc |
| `Glue.Lib.Math.Utility.Min.hs` | `Glue.Lib.Math.Utility.MinSpec.hs` | min |
| `Glue.Lib.Math.Utility.Max.hs` | `Glue.Lib.Math.Utility.MaxSpec.hs` | max |

### Dart Modules & Files

#### Bool Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/bool.dart` | - | - |
| `lib/src/lib/bool/eq.dart` | `test/lib/bool/eq_test.dart` | eq |
| `lib/src/lib/bool/ne.dart` | `test/lib/bool/ne_test.dart` | ne |
| `lib/src/lib/bool/lt.dart` | `test/lib/bool/lt_test.dart` | lt |
| `lib/src/lib/bool/le.dart` | `test/lib/bool/le_test.dart` | le |
| `lib/src/lib/bool/gt.dart` | `test/lib/bool/gt_test.dart` | gt |
| `lib/src/lib/bool/ge.dart` | `test/lib/bool/ge_test.dart` | ge |
| `lib/src/lib/bool/not.dart` | `test/lib/bool/not_test.dart` | not |
| `lib/src/lib/bool/if.dart` | `test/lib/bool/if_test.dart` | if |
| `lib/src/lib/bool/when.dart` | `test/lib/bool/when_test.dart` | when |
| `lib/src/lib/bool/while.dart` | `test/lib/bool/while_test.dart` | while |
| - | `test/lib/bool/until_test.dart` | until (test exists, source missing) |

#### IO Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/io.dart` | - | - |
| `lib/src/lib/io/print.dart` | `test/lib/io/print_test.dart` | print, println |
| `lib/src/lib/io/read.dart` | - | read-line |

#### Builtin Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/builtin.dart` | - | - |
| `lib/src/lib/builtin/def.dart` | `test/lib/builtin/def_test.dart` | def |
| `lib/src/lib/builtin/set.dart` | `test/lib/builtin/set_test.dart` | set |
| `lib/src/lib/builtin/lambda.dart` | `test/lib/builtin/lambda_test.dart` | lambda |
| `lib/src/lib/builtin/let.dart` | - | let |
| `lib/src/lib/builtin/import.dart` | - | import |
| `lib/src/lib/builtin/try.dart` | `test/lib/builtin/try_test.dart` | try |
| `lib/src/lib/builtin/error.dart` | - | error |

#### List Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/list.dart` | - | - |
| `lib/src/lib/list/append.dart` | `test/lib/list/append_test.dart` | append |
| `lib/src/lib/list/butlast.dart` | `test/lib/list/butlast_test.dart` | butlast |
| `lib/src/lib/list/car.dart` | `test/lib/list/car_test.dart` | car |
| `lib/src/lib/list/cdr.dart` | `test/lib/list/cdr_test.dart` | cdr |
| `lib/src/lib/list/cons.dart` | `test/lib/list/cons_test.dart` | cons |
| `lib/src/lib/list/drop.dart` | `test/lib/list/drop_test.dart` | drop |
| `lib/src/lib/list/filter.dart` | `test/lib/list/filter_test.dart` | filter |
| `lib/src/lib/list/find.dart` | `test/lib/list/find_test.dart` | find |
| `lib/src/lib/list/flatten.dart` | `test/lib/list/flatten_test.dart` | flatten |
| `lib/src/lib/list/last.dart` | `test/lib/list/last_test.dart` | last |
| `lib/src/lib/list/length.dart` | `test/lib/list/length_test.dart` | length |
| `lib/src/lib/list/map.dart` | `test/lib/list/map_test.dart` | map |
| `lib/src/lib/list/member.dart` | `test/lib/list/member_test.dart` | member |
| `lib/src/lib/list/nth.dart` | `test/lib/list/nth_test.dart` | nth |
| `lib/src/lib/list/partition.dart` | `test/lib/list/partition_test.dart` | partition |
| `lib/src/lib/list/position.dart` | `test/lib/list/position_test.dart` | position |
| `lib/src/lib/list/remove.dart` | `test/lib/list/remove_test.dart` | remove |
| `lib/src/lib/list/reverse.dart` | `test/lib/list/reverse_test.dart` | reverse |
| `lib/src/lib/list/sort.dart` | `test/lib/list/sort_test.dart` | sort |
| `lib/src/lib/list/take.dart` | `test/lib/list/take_test.dart` | take |
| `lib/src/lib/list/zip.dart` | `test/lib/list/zip_test.dart` | zip |

#### Math Module - Arithmetic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/arithmetic/add.dart` | `test/lib/math/arithmetic/add_test.dart` | add |
| `lib/src/lib/math/arithmetic/sub.dart` | `test/lib/math/arithmetic/sub_test.dart` | sub |
| `lib/src/lib/math/arithmetic/mul.dart` | `test/lib/math/arithmetic/mul_test.dart` | mul |
| `lib/src/lib/math/arithmetic/div.dart` | `test/lib/math/arithmetic/div_test.dart` | div |
| `lib/src/lib/math/arithmetic/mod.dart` | `test/lib/math/arithmetic/mod_test.dart` | mod |

#### Math Module - Power Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/power/exp.dart` | `test/lib/math/power/exp_test.dart` | exp |
| `lib/src/lib/math/power/pow.dart` | `test/lib/math/power/pow_test.dart` | pow |
| `lib/src/lib/math/power/sqrt.dart` | `test/lib/math/power/sqrt_test.dart` | sqrt |

#### Math Module - Trigonometric Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/trigonometric/sin.dart` | `test/lib/math/trigonometric/sin_test.dart` | sin |
| `lib/src/lib/math/trigonometric/cos.dart` | `test/lib/math/trigonometric/cos_test.dart` | cos |
| `lib/src/lib/math/trigonometric/tan.dart` | `test/lib/math/trigonometric/tan_test.dart` | tan |
| `lib/src/lib/math/trigonometric/asin.dart` | `test/lib/math/trigonometric/asin_test.dart` | asin |
| `lib/src/lib/math/trigonometric/acos.dart` | `test/lib/math/trigonometric/acos_test.dart` | acos |
| `lib/src/lib/math/trigonometric/atan.dart` | `test/lib/math/trigonometric/atan_test.dart` | atan |

#### Math Module - Logarithmic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/logarithmic/log.dart` | `test/lib/math/logarithmic/log_test.dart` | log |
| `lib/src/lib/math/logarithmic/ln.dart` | `test/lib/math/logarithmic/ln_test.dart` | ln |
| `lib/src/lib/math/logarithmic/lg.dart` | `test/lib/math/logarithmic/lg_test.dart` | lg |

#### Math Module - Utility Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/utility/abs.dart` | `test/lib/math/utility/abs_test.dart` | abs |
| `lib/src/lib/math/utility/ceil.dart` | `test/lib/math/utility/ceil_test.dart` | ceil |
| `lib/src/lib/math/utility/floor.dart` | `test/lib/math/utility/floor_test.dart` | floor |
| `lib/src/lib/math/utility/round.dart` | `test/lib/math/utility/round_test.dart` | round |
| `lib/src/lib/math/utility/trunc.dart` | `test/lib/math/utility/trunc_test.dart` | trunc |
| `lib/src/lib/math/utility/min.dart` | `test/lib/math/utility/min_test.dart` | min |
| `lib/src/lib/math/utility/max.dart` | `test/lib/math/utility/max_test.dart` | max |

## Implementation Phase 1: Constructor Migration (Steps 1-6)
**Goal:** Move constructors from ModuleInfo to function implementations for ALL modules

#### Haskell Implementation (Steps 1-3)

**Step 1: Bool Module**
- **Files to modify**: `Glue.Lib.Bool.hs` (remove constructors from module), `Glue.Lib.Bool.*.hs` (11 files: add constructors to functions)
- **Test specs to update**: `Glue.Lib.Bool.*Spec.hs` (11 files: update to use `apply` instead of extracting functions)
- **Functions affected**: eq, ne, lt, le, gt, ge, not, if, when, while, until

**Step 2: IO Module**
- **Files to modify**: `Glue.Lib.IO.hs` (remove constructors), `Glue.Lib.IO.Print.hs`, `Glue.Lib.IO.Read.hs` (add constructors)
- **Test specs to update**: `Glue.Lib.IO.PrintSpec.hs`
- **Functions affected**: print, println, read-line

**Step 3: Builtin Module**
- **Files to modify**: `Glue.Lib.Builtin.hs` (remove constructors), `Glue.Lib.Builtin.*.hs` (7 files: add constructors)
- **Test specs to update**: `Glue.Lib.Builtin.*Spec.hs` (5 files: update tests)
- **Functions affected**: def, set, lambda, let, import, try, error

**Step 4: List Module**
- **Files to modify**: `Glue.Lib.List.hs` (remove constructors), `Glue.Lib.List.*.hs` (20 files: add constructors)
- **Test specs to update**: `Glue.Lib.List.*Spec.hs` (21 files: update tests)
- **Functions affected**: append, butlast, car, cdr, cons, drop, filter, find, flatten, last, length, map, member, nth, partition, position, remove, reverse, sort, take, zip

**Step 5: Math Module**
- **Files to modify**:
  - Main: `Glue.Lib.Math.hs`, `Glue.Lib.Math.Arithmetic.hs`, `Glue.Lib.Math.Power.hs`, `Glue.Lib.Math.Trigonometric.hs`, `Glue.Lib.Math.Logarithmic.hs`, `Glue.Lib.Math.Utility.hs`
  - Arithmetic: `Glue.Lib.Math.Arithmetic.*.hs` (5 files)
  - Power: `Glue.Lib.Math.Power.*.hs` (3 files)
  - Trigonometric: `Glue.Lib.Math.Trigonometric.*.hs` (6 files)
  - Logarithmic: `Glue.Lib.Math.Logarithmic.*.hs` (3 files)
  - Utility: `Glue.Lib.Math.Utility.*.hs` (7 files)
- **Test specs to update**: `Glue.Lib.Math.*.*Spec.hs` (organized by submodules)
- **Functions affected**: add, sub, mul, div, mod, pow, sqrt, exp, sin, cos, tan, asin, acos, atan, log, ln, lg, abs, ceil, floor, round, trunc, min, max

**Step 6: Final Verification**
- **Run all tests**: Execute complete test suite
- **Verify functionality**: Ensure all modules work correctly

#### Dart Implementation (Steps 1-3 - Sync)
Make identical changes in Dart implementation in same order (1-6)
- **Bool**: `lib/src/lib/bool.dart`, `lib/src/lib/bool/*.dart` (11 files)
- **IO**: `lib/src/lib/io.dart`
- **Builtin**: `lib/src/lib/builtin.dart`
- **List**: `lib/src/lib/list.dart`
- **Math**: `lib/src/lib/math.dart`, `lib/src/lib/math/*/` (submodules)

**See:** [Development Technology](development-technology.md) for cross-language synchronization requirements

## Success Criteria for Phase 1

- ✅ All `NativeFunc`/`Special` constructors moved from ModuleInfo to function implementations
- ✅ All test specs updated to use `apply` instead of direct function calls
- ✅ All modules compile and pass tests
- ✅ Haskell and Dart implementations synchronized
- ✅ Ready for Phase 2: Currying Implementation

## Rationale

Phase 1 establishes the foundation by moving all constructors to function implementations, enabling Phase 2 to change the function signatures and implement currying logic.

**See:** [Partial Application Phase 2 Plan](partial-application-phase2-plan.md) for the next phase
