# Partial Application Implementation Plan

## Overview
Implement partial application (currying) support for native functions to make Glue a proper functional programming language with first-class functions.

## Current Issue
`NativeFunc` doesn't support partial application while `Closure` does, breaking functional programming principles.

## Solution: Universal Currying Contract
- **Single contract for ALL functions:** `IR → IR`
- Functions take one argument, return result or closure
- Automatic partial application (Haskell-style currying)
- No arity declarations or complex logic

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
| `lib/src/lib/io/print.dart` | - | print, println |
| `lib/src/lib/io/read.dart` | - | read-line |

#### Builtin Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/builtin.dart` | - | - |
| `lib/src/lib/builtin/def.dart` | - | def |
| `lib/src/lib/builtin/set.dart` | - | set |
| `lib/src/lib/builtin/lambda.dart` | - | lambda |
| `lib/src/lib/builtin/let.dart` | - | let |
| `lib/src/lib/builtin/import.dart` | - | import |
| `lib/src/lib/builtin/try.dart` | - | try |
| `lib/src/lib/builtin/error.dart` | - | error |

#### List Module
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/list.dart` | - | - |
| `lib/src/lib/list/append.dart` | - | append |
| `lib/src/lib/list/butlast.dart` | - | butlast |
| `lib/src/lib/list/car.dart` | - | car |
| `lib/src/lib/list/cdr.dart` | - | cdr |
| `lib/src/lib/list/cons.dart` | - | cons |
| `lib/src/lib/list/drop.dart` | - | drop |
| `lib/src/lib/list/filter.dart` | - | filter |
| `lib/src/lib/list/find.dart` | - | find |
| `lib/src/lib/list/flatten.dart` | - | flatten |
| `lib/src/lib/list/last.dart` | - | last |
| `lib/src/lib/list/length.dart` | - | length |
| `lib/src/lib/list/map.dart` | - | map |
| `lib/src/lib/list/member.dart` | - | member |
| `lib/src/lib/list/nth.dart` | - | nth |
| `lib/src/lib/list/partition.dart` | - | partition |
| `lib/src/lib/list/position.dart` | - | position |
| `lib/src/lib/list/remove.dart` | - | remove |
| `lib/src/lib/list/reverse.dart` | - | reverse |
| `lib/src/lib/list/sort.dart` | - | sort |
| `lib/src/lib/list/take.dart` | - | take |
| `lib/src/lib/list/zip.dart` | - | zip |

#### Math Module - Arithmetic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/arithmetic/add.dart` | - | add |
| `lib/src/lib/math/arithmetic/sub.dart` | - | sub |
| `lib/src/lib/math/arithmetic/mul.dart` | - | mul |
| `lib/src/lib/math/arithmetic/div.dart` | - | div |
| `lib/src/lib/math/arithmetic/mod.dart` | - | mod |

#### Math Module - Power Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/power/exp.dart` | - | exp |
| `lib/src/lib/math/power/pow.dart` | - | pow |
| `lib/src/lib/math/power/sqrt.dart` | - | sqrt |

#### Math Module - Trigonometric Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/trigonometric/sin.dart` | - | sin |
| `lib/src/lib/math/trigonometric/cos.dart` | - | cos |
| `lib/src/lib/math/trigonometric/tan.dart` | - | tan |
| `lib/src/lib/math/trigonometric/asin.dart` | - | asin |
| `lib/src/lib/math/trigonometric/acos.dart` | - | acos |
| `lib/src/lib/math/trigonometric/atan.dart` | - | atan |

#### Math Module - Logarithmic Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/logarithmic/log.dart` | - | log |
| `lib/src/lib/math/logarithmic/ln.dart` | - | ln |
| `lib/src/lib/math/logarithmic/lg.dart` | - | lg |

#### Math Module - Utility Submodule
| Source File | Test Spec | Function |
|-------------|-----------|----------|
| `lib/src/lib/math/utility/abs.dart` | - | abs |
| `lib/src/lib/math/utility/ceil.dart` | - | ceil |
| `lib/src/lib/math/utility/floor.dart` | - | floor |
| `lib/src/lib/math/utility/round.dart` | - | round |
| `lib/src/lib/math/utility/trunc.dart` | - | trunc |
| `lib/src/lib/math/utility/min.dart` | - | min |
| `lib/src/lib/math/utility/max.dart` | - | max |

## Implementation Phases

### Phase 1: Constructor Migration (Steps 1-6)
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

### Phase 2: Currying Implementation (Steps 4-12)
**Goal:** Change NativeFunc to single-argument contract and implement currying

#### Haskell Implementation

**Step 4: Change NativeFunc Type**
- **Files to modify**: `Glue/IR.hs` (change `NativeFunc` type signature)
- **No test changes needed**

**Step 5: Update Evaluator**
- **Files to modify**: `Glue/Eval.hs` (change `apply` function to single argument, update `applyFunction`)
- **No test changes needed**

**Step 6: Rewrite Bool Functions for Currying**
- **Files to modify**: `Glue.Lib.Bool.*.hs` (11 files: implement currying logic)
- **Test specs to update**: `Glue.Lib.Bool.*Spec.hs` (11 files: update to use single arguments)

**Step 7: Rewrite IO Functions for Currying**
- **Files to modify**: `Glue.Lib.IO.Print.hs`, `Glue.Lib.IO.Read.hs` (implement currying)
- **Test specs to update**: `Glue.Lib.IO.PrintSpec.hs`

**Step 8: Rewrite Builtin Functions for Currying**
- **Files to modify**: `Glue.Lib.Builtin.*.hs` (7 files: implement currying)
- **Test specs to update**: `Glue.Lib.Builtin.*Spec.hs` (5 files: update tests)

**Step 9: Rewrite List Functions for Currying**
- **Files to modify**: `Glue.Lib.List.*.hs` (20 files: implement currying)
- **Test specs to update**: `Glue.Lib.List.*Spec.hs` (21 files: update tests)

**Step 10: Rewrite Math Functions for Currying**
- **Files to modify**:
  - Arithmetic: `Glue.Lib.Math.Arithmetic.*.hs` (5 files)
  - Power: `Glue.Lib.Math.Power.*.hs` (3 files)
  - Trigonometric: `Glue.Lib.Math.Trigonometric.*.hs` (6 files)
  - Logarithmic: `Glue.Lib.Math.Logarithmic.*.hs` (3 files)
  - Utility: `Glue.Lib.Math.Utility.*.hs` (7 files)
- **Test specs to update**: `Glue.Lib.Math.*.*Spec.hs` (organized by submodules)

**Step 11: Add Currying Tests**
- **Files to modify**: `Glue/EvalSpec.hs` (add currying test cases)
- **Run tests**: Verify currying works (`((+ 1) 2)` etc.)

**Step 12: Final Verification**
- **Run all tests**: Execute complete test suite
- **Fix any bugs**: Address issues found in testing
- **Commit final changes**

#### Dart Implementation
Make changes in the Dart implementation in the same order from 4 to 12
- **IR**: `lib/src/ir.dart` (change NativeFunc type)
- **Eval**: `lib/src/eval.dart` (update apply function)
- **All module files**: Implement currying in all function files
- **All test files**: Update test specs

**See:** [Development Technology](development-technology.md) for cross-language synchronization requirements

### Documentation
Update drafts and specifications
Make commit

**See:** [Implementation Verification](implementation-verification.md) for testing and validation procedures

## Key Technical Decisions

- **Universal contract**: All functions `IR → IR` (single argument)
- **Internal currying**: Functions decide when to return result vs closure
- **Automatic partial application**: Every call can be partial
- **Special forms**: No partial application (syntactic constructs)
- **Pure functional**: Haskell-style evaluation model

## Success Criteria

- ✅ `((+ 1) 2)` returns `3`
- ✅ `((cons 1) (2 3 4))` works for lists
- ✅ `((print "hello") "world")` prints both strings
- ✅ Performance: Simple single-argument application
- ✅ Pure functional: Haskell-style currying throughout
- ✅ Cross-implementation: Haskell and Dart behave identically

## Rationale

This approach:
- Maintains functional programming principles
- Provides zero-overhead partial application
- Reuses existing infrastructure
- Enables code like `((+ 1) 2)` and `((map f) list)`
- Works for both positional and named argument functions
