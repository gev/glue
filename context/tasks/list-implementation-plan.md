# List Module Implementation Plan

## Overview
Implement the complete List module in Dart following Haskell reference implementation, adhering to development-technology.md and implementation-verification.md guidelines.

## Haskell Reference Analysis

### Haskell Source Modules (20 modules)
Located in: `haskell/glue/src/Glue/Lib/List/`

| Haskell Module | Description |
|----------------|-------------|
| `Append.hs` | List concatenation |
| `Butlast.hs` | All elements except last |
| `Car.hs` | First element of list |
| `Cdr.hs` | Rest of list after first element |
| `Cons.hs` | Construct new list with element |
| `Drop.hs` | Remove first N elements |
| `Filter.hs` | Filter elements by predicate |
| `Find.hs` | Find first element matching predicate |
| `Flatten.hs` | Flatten nested lists |
| `Last.hs` | Last element of list |
| `Length.hs` | List length |
| `Map.hs` | Apply function to each element |
| `Member.hs` | Check if element is in list |
| `Nth.hs` | Get Nth element |
| `Partition.hs` | Split list by predicate |
| `Position.hs` | Find position of element |
| `Remove.hs` | Remove elements by value |
| `Reverse.hs` | Reverse list order |
| `Sort.hs` | Sort list elements |
| `Take.hs` | Take first N elements |
| `Zip.hs` | Combine two lists |

### Haskell Test Modules (20 test modules)
Located in: `haskell/glue/test/Glue/Lib/List/`

| Haskell Test | Corresponding Source |
|--------------|---------------------|
| `AppendSpec.hs` | `Append.hs` |
| `ButlastSpec.hs` | `Butlast.hs` |
| `CarSpec.hs` | `Car.hs` |
| `CdrSpec.hs` | `Cdr.hs` |
| `ConsSpec.hs` | `Cons.hs` |
| `DropSpec.hs` | `Drop.hs` |
| `FilterSpec.hs` | `Filter.hs` |
| `FindSpec.hs` | `Find.hs` |
| `FlattenSpec.hs` | `Flatten.hs` |
| `LastSpec.hs` | `Last.hs` |
| `LengthSpec.hs` | `Length.hs` |
| `MapSpec.hs` | `Map.hs` |
| `MemberSpec.hs` | `Member.hs` |
| `NthSpec.hs` | `Nth.hs` |
| `PartitionSpec.hs` | `Partition.hs` |
| `PositionSpec.hs` | `Position.hs` |
| `RemoveSpec.hs` | `Remove.hs` |
| `ReverseSpec.hs` | `Reverse.hs` |
| `SortSpec.hs` | `Sort.hs` |
| `TakeSpec.hs` | `Take.hs` |
| `ZipSpec.hs` | `Zip.hs` |

## Dart Implementation Plan

### Dart Source Modules (20 modules)
To be created in: `dart/glue/lib/src/lib/list/`

| Haskell Module | Dart Module | Status |
|----------------|-------------|--------|
| `Append.hs` | `append.dart` | ⏳ TODO |
| `Butlast.hs` | `butlast.dart` | ⏳ TODO |
| `Car.hs` | `car.dart` | ⏳ TODO |
| `Cdr.hs` | `cdr.dart` | ⏳ TODO |
| `Cons.hs` | `cons.dart` | ⏳ TODO |
| `Drop.hs` | `drop.dart` | ⏳ TODO |
| `Filter.hs` | `filter.dart` | ⏳ TODO |
| `Find.hs` | `find.dart` | ⏳ TODO |
| `Flatten.hs` | `flatten.dart` | ⏳ TODO |
| `Last.hs` | `last.dart` | ⏳ TODO |
| `Length.hs` | `length.dart` | ⏳ TODO |
| `Map.hs` | `map.dart` | ⏳ TODO |
| `Member.hs` | `member.dart` | ⏳ TODO |
| `Nth.hs` | `nth.dart` | ⏳ TODO |
| `Partition.hs` | `partition.dart` | ⏳ TODO |
| `Position.hs` | `position.dart` | ⏳ TODO |
| `Remove.hs` | `remove.dart` | ⏳ TODO |
| `Reverse.hs` | `reverse.dart` | ⏳ TODO |
| `Sort.hs` | `sort.dart` | ⏳ TODO |
| `Take.hs` | `take.dart` | ⏳ TODO |
| `Zip.hs` | `zip.dart` | ⏳ TODO |

### Dart Test Modules (20 test modules)
To be created in: `dart/glue/test/lib/list/`

| Haskell Test | Dart Test | Status |
|--------------|-----------|--------|
| `AppendSpec.hs` | `append_test.dart` | ⏳ TODO |
| `ButlastSpec.hs` | `butlast_test.dart` | ⏳ TODO |
| `CarSpec.hs` | `car_test.dart` | ⏳ TODO |
| `CdrSpec.hs` | `cdr_test.dart` | ⏳ TODO |
| `ConsSpec.hs` | `cons_test.dart` | ⏳ TODO |
| `DropSpec.hs` | `drop_test.dart` | ⏳ TODO |
| `FilterSpec.hs` | `filter_test.dart` | ⏳ TODO |
| `FindSpec.hs` | `find_test.dart` | ⏳ TODO |
| `FlattenSpec.hs` | `flatten_test.dart` | ⏳ TODO |
| `LastSpec.hs` | `last_test.dart` | ⏳ TODO |
| `LengthSpec.hs` | `length_test.dart` | ⏳ TODO |
| `MapSpec.hs` | `map_test.dart` | ⏳ TODO |
| `MemberSpec.hs` | `member_test.dart` | ⏳ TODO |
| `NthSpec.hs` | `nth_test.dart` | ⏳ TODO |
| `PartitionSpec.hs` | `partition_test.dart` | ⏳ TODO |
| `PositionSpec.hs` | `position_test.dart` | ⏳ TODO |
| `RemoveSpec.hs` | `remove_test.dart` | ⏳ TODO |
| `ReverseSpec.hs` | `reverse_test.dart` | ⏳ TODO |
| `SortSpec.hs` | `sort_test.dart` | ⏳ TODO |
| `TakeSpec.hs` | `take_test.dart` | ⏳ TODO |
| `ZipSpec.hs` | `zip_test.dart` | ⏳ TODO |

## Implementation Strategy

### Phase 1: Core List Operations (Basic)
**Priority: High** - Fundamental list operations

1. **Car/Cdr Operations** ✅ COMPLETED (2 modules)
   - [x] `car.dart` + `car_test.dart`
   - [x] `cdr.dart` + `cdr_test.dart`

2. **Construction Operations** ✅ COMPLETED (2/2 modules)
   - [x] `cons.dart` + `cons_test.dart`
   - [x] `append.dart` + `append_test.dart`

3. **Basic Properties** ✅ COMPLETED (2/2 modules)
   - [x] `length.dart` + `length_test.dart`
   - [x] `last.dart` + `last_test.dart`

### Phase 2: List Manipulation (Medium)
**Priority: High** - Essential list processing

4. **Slicing Operations** ✅ COMPLETED (4/4 modules)
   - [x] `take.dart` + `take_test.dart`
   - [x] `drop.dart` + `drop_test.dart`
   - [x] `butlast.dart` + `butlast_test.dart`
   - [x] `nth.dart` + `nth_test.dart`

5. **Order Operations** (2 modules)
   - [ ] `reverse.dart` + `reverse_test.dart`
   - [ ] `sort.dart` + `sort_test.dart`

### Phase 3: Functional Programming (Medium)
**Priority: Medium** - Higher-order functions

6. **Mapping Operations** (2 modules)
   - [ ] `map.dart` + `map_test.dart`
   - [ ] `filter.dart` + `filter_test.dart`

7. **Search Operations** (3 modules)
   - [ ] `find.dart` + `find_test.dart`
   - [ ] `member.dart` + `member_test.dart`
   - [ ] `position.dart` + `position_test.dart`

### Phase 4: Advanced Operations (Low)
**Priority: Low** - Specialized operations

8. **Set Operations** (2 modules)
   - [ ] `remove.dart` + `remove_test.dart`
   - [ ] `partition.dart` + `partition_test.dart`

9. **Combination Operations** (2 modules)
   - [ ] `zip.dart` + `zip_test.dart`
   - [ ] `flatten.dart` + `flatten_test.dart`

### Phase 5: Integration & Verification
**Priority: High** - Complete implementation

10. **Module Integration**
    - [ ] Create `list.dart` main module file
    - [ ] Update `eval_test.dart` to include list functions
    - [ ] Verify all functions work in Glue runtime

11. **Final Verification**
    - [ ] Run complete test suite (380+ tests expected)
    - [ ] Verify structural compliance with Haskell
    - [ ] Update implementation plan with completion status

## Haskell/Dart Correspondence Table

| Category | Haskell Source | Haskell Test | Dart Source | Dart Test | Status |
|----------|----------------|--------------|-------------|-----------|--------|
| **Core** | `Car.hs` | `CarSpec.hs` | `car.dart` | `car_test.dart` | ✅ DONE |
| **Core** | `Cdr.hs` | `CdrSpec.hs` | `cdr.dart` | `cdr_test.dart` | ✅ DONE |
| **Core** | `Cons.hs` | `ConsSpec.hs` | `cons.dart` | `cons_test.dart` | ✅ DONE |
| **Core** | `Length.hs` | `LengthSpec.hs` | `length.dart` | `length_test.dart` | ✅ DONE |
| **Core** | `Append.hs` | `AppendSpec.hs` | `append.dart` | `append_test.dart` | ✅ DONE |
| **Core** | `Last.hs` | `LastSpec.hs` | `last.dart` | `last_test.dart` | ✅ DONE |
| **Slice** | `Take.hs` | `TakeSpec.hs` | `take.dart` | `take_test.dart` | ✅ DONE |
| **Slice** | `Drop.hs` | `DropSpec.hs` | `drop.dart` | `drop_test.dart` | ✅ DONE |
| **Slice** | `Butlast.hs` | `ButlastSpec.hs` | `butlast.dart` | `butlast_test.dart` | ✅ DONE |
| **Slice** | `Nth.hs` | `NthSpec.hs` | `nth.dart` | `nth_test.dart` | ✅ DONE |
| **Order** | `Reverse.hs` | `ReverseSpec.hs` | `reverse.dart` | `reverse_test.dart` | ⏳ TODO |
| **Order** | `Sort.hs` | `SortSpec.hs` | `sort.dart` | `sort_test.dart` | ⏳ TODO |
| **Func** | `Map.hs` | `MapSpec.hs` | `map.dart` | `map_test.dart` | ⏳ TODO |
| **Func** | `Filter.hs` | `FilterSpec.hs` | `filter.dart` | `filter_test.dart` | ⏳ TODO |
| **Search** | `Find.hs` | `FindSpec.hs` | `find.dart` | `find_test.dart` | ⏳ TODO |
| **Search** | `Member.hs` | `MemberSpec.hs` | `member.dart` | `member_test.dart` | ⏳ TODO |
| **Search** | `Position.hs` | `PositionSpec.hs` | `position.dart` | `position_test.dart` | ⏳ TODO |
| **Set** | `Remove.hs` | `RemoveSpec.hs` | `remove.dart` | `remove_test.dart` | ⏳ TODO |
| **Set** | `Partition.hs` | `PartitionSpec.hs` | `partition.dart` | `partition_test.dart` | ⏳ TODO |
| **Combo** | `Zip.hs` | `ZipSpec.hs` | `zip.dart` | `zip_test.dart` | ⏳ TODO |
| **Combo** | `Flatten.hs` | `FlattenSpec.hs` | `flatten.dart` | `flatten_test.dart` | ⏳ TODO |

## Implementation Requirements

### Structural Compliance
- **Directory Structure**: Must mirror Haskell exactly
- **File Naming**: `ModuleName.hs` → `module_name.dart`
- **Test Naming**: `FunctionSpec.hs` → `function_test.dart`
- **One-to-one Mapping**: Every Haskell file must have Dart equivalent

### Behavioral Fidelity
- **Function Signatures**: Must match Haskell exactly
- **Error Handling**: Same error conditions and messages
- **Type Handling**: Proper Integer/Float/List distinctions
- **Edge Cases**: Handle empty lists, invalid indices, etc.

### Testing Requirements
- **100% Coverage**: Every function tested against Haskell
- **Test Structure**: Mirror Haskell test organization
- **Behavioral Verification**: Same inputs produce same outputs

## Success Criteria

- [ ] **20 Dart modules** implemented with Haskell fidelity
- [ ] **20 Dart test modules** with comprehensive coverage
- [ ] **Structural compliance** with Haskell organization
- [ ] **All tests pass** with correct mathematical behavior
- [ ] **Integration verified** in Glue runtime
- [ ] **Documentation complete** with Haskell references

## Timeline Estimate

- **Phase 1**: 1 week (6 core functions)
- **Phase 2**: 1 week (6 manipulation functions)
- **Phase 3**: 1 week (5 functional programming functions)
- **Phase 4**: 1 week (3 advanced functions)
- **Phase 5**: 1 week (integration & verification)

**Total: 5 weeks for complete List module implementation**

## Risk Assessment

- **Complexity**: List operations vary significantly in complexity
- **Edge Cases**: Many functions need careful handling of empty lists, bounds checking
- **Performance**: Some operations (sort, filter) may need optimization
- **Type Safety**: Dart's type system differs from Haskell's

## Dependencies

- **Bool Module**: Already completed ✅
- **Math Module**: Already completed ✅
- **Core Runtime**: Must support list operations
- **Test Framework**: Dart test package
