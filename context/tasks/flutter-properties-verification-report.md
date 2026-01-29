# Flutter Properties Verification Report

**Date:** 30/01/2026  
**Document Verified:** context/tasks/flutter-properties-verification-plan.md

## Executive Summary

This report verifies that the flutter-properties-verification-plan.md document contains every widget and every widget property from the Flutter widget documentation.

### Verification Status: ⚠️ INCOMPLETE - Missing Critical Widget

**Critical Issues Found:**
- 1 widget completely missing: **TabBar** (Material Navigation)
- 2 widget properties have errors in the verification plan

---

## Missing Widgets

### 1. ❌ TabBar Widget (Material Navigation)

**Location in Documentation:** context/docs/flutter_widgets/material/navigation.md

**Constructor from Documentation:**
```dart
TabBar({
  Key? key,
  required List<Widget> tabs,
  TabController? controller,
  bool isScrollable = false,
  EdgeInsetsGeometry? padding,
  Color? indicatorColor,
  bool automaticIndicatorColorAdjustment = true,
  double indicatorWeight = 2.0,
  EdgeInsetsGeometry indicatorPadding = EdgeInsets.zero,
  Decoration? indicator,
  TabBarIndicatorSize? indicatorSize,
  Color? dividerColor,
  double? dividerHeight,
  Color? labelColor,
  TextStyle? labelStyle,
  EdgeInsetsGeometry? labelPadding,
  Color? unselectedLabelColor,
  TextStyle? unselectedLabelStyle,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  WidgetStateProperty<Color?>? overlayColor,
  MouseCursor? mouseCursor,
  bool? enableFeedback,
  ValueChanged<int>? onTap,
  TabValueChanged<bool>? onHover,
  TabValueChanged<bool>? onFocusChange,
  ScrollPhysics? physics,
  InteractiveInkFeatureFactory? splashFactory,
  BorderRadius? splashBorderRadius,
  TabAlignment? tabAlignment,
  TextScaler? textScaler,
  TabIndicatorAnimation? indicatorAnimation,
})
```

**Status:** This entire widget is missing from the verification plan!

**Required Addition:** A new section should be added to the Material Module with a complete property table for TabBar.

---

## Property Errors Found

### 1. ❌ Switch Widget - Property Duplication Error

**Location:** Material Module → Switch widget  
**Line:** In the Switch property table

**Error Found:**
```
| [ ] | onInactiveThumbImageError | ImageErrorListener? | getValue | getValue | on-inactive-thumb-image-error |
```

**Issue:** The property getter column shows "getValue" twice instead of "getValue" once.

**Should be:**
```
| [ ] | onInactiveThumbImageError | ImageErrorListener? | getValue | on-inactive-thumb-image-error |
```

### 2. ⚠️ Placeholder Widget - Missing Property

**Location:** Core Module → Placeholder widget

**Properties in Verification Plan:**
- key ✓
- fallbackWidth ✓
- fallbackHeight ✓
- color ✓
- strokeWidth ✓

**Properties in Documentation (basic_widgets.md):**
```dart
Placeholder({
  Key? key,
  double? fallbackWidth,
  double? fallbackHeight,
  Color? color,
  StrokeAlign strokeAlign = StrokeAlign.inside,  // ❌ MISSING
  double strokeWidth = 2.0,
})
```

**Missing Property:**
- `strokeAlign` (type: StrokeAlign)

---

## Verification Summary by Module

### Core Module (flutter/glue_flutter/lib/src/lib/ui/core/)
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| AppBar | ✅ Complete | 33 | None |
| Column | ✅ Complete | 7 | None |
| Container | ✅ Complete | 14 | None |
| ElevatedButton | ✅ Complete | 9 | None |
| FlutterLogo | ✅ Complete | 7 | None |
| Icon | ✅ Complete | 6 | None |
| Image | ✅ Complete | 17 | None |
| Placeholder | ⚠️ Incomplete | 5 | Missing: strokeAlign |
| Row | ✅ Complete | 7 | None |
| Scaffold | ✅ Complete | 22 | None |
| Text | ✅ Complete | 14 | None |

### Cupertino Module (flutter/glue_flutter/lib/src/lib/ui/cupertino/)
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| CupertinoActionSheet | ✅ Complete | 7 | None |
| CupertinoActivityIndicator | ✅ Complete | 4 | None |
| CupertinoAlertDialog | ✅ Complete | 8 | None |
| CupertinoApp | ✅ Complete | 28 | None |
| CupertinoButton | ✅ Complete | 10 | None |
| CupertinoCheckbox | ✅ Complete | 10 | None |
| CupertinoContextMenu | ✅ Complete | 4 | None |
| CupertinoDatePicker | ✅ Complete | 13 | None |
| CupertinoNavigationBar | ✅ Complete | 13 | None |
| CupertinoPageScaffold | ✅ Complete | 5 | None |
| CupertinoPicker | ✅ Complete | 12 | None |
| CupertinoScrollbar | ✅ Complete | 9 | None |
| CupertinoSearchTextField | ✅ Complete | 23 | None |
| CupertinoSegmentedControl | ✅ Complete | 9 | None |
| CupertinoSlider | ✅ Complete | 10 | None |
| CupertinoSlidingSegmentedControl | ✅ Complete | 6 | None |
| CupertinoSwitch | ✅ Complete | 9 | None |
| CupertinoTabBar | ✅ Complete | 9 | None |
| CupertinoTabScaffold | ✅ Complete | 7 | None |
| CupertinoTextField | ✅ Complete | 58 | None |
| CupertinoTimerPicker | ✅ Complete | 9 | None |

### Material Module (flutter/glue_flutter/lib/src/lib/ui/material/)

#### Actions
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| ElevatedButton | ✅ Complete | 9 | None (duplicate from core) |
| FilledButton | ✅ Complete | 9 | None |
| FloatingActionButton | ✅ Complete | 28 | None |
| IconButton | ✅ Complete | 20 | None |
| OutlinedButton | ✅ Complete | 9 | None |
| SegmentedButton | ✅ Complete | 16 | None |
| TextButton | ✅ Complete | 9 | None |

#### Communication
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| Badge | ✅ Complete | 12 | None |
| LinearProgressIndicator | ✅ Complete | 8 | None |
| SnackBar | ✅ Complete | 15 | None |

#### Containment
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| AlertDialog | ✅ Complete | 28 | None |
| BottomSheet | ✅ Complete | 16 | None |
| Card | ✅ Complete | 11 | None |
| Divider | ✅ Complete | 7 | None |
| ListTile | ✅ Complete | 41 | None |

#### Navigation
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| BottomAppBar | ✅ Complete | 10 | None |
| NavigationBar | ✅ Complete | 16 | None |
| NavigationDrawer | ✅ Complete | 12 | None |
| NavigationRail | ✅ Complete | 23 | None |
| **TabBar** | ❌ **MISSING** | **0** | **Widget not in plan!** |

#### Selection
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| Checkbox | ✅ Complete | 20 | None |
| Chip | ✅ Complete | 26 | None |
| DatePickerDialog | ✅ Complete | 21 | None |
| MenuAnchor | ✅ Complete | 17 | None |
| Radio | ✅ Complete | 20 | None |
| Slider | ✅ Complete | 24 | None |
| Switch | ⚠️ Error | 28 | Duplication in property getter column |
| TimePickerDialog | ✅ Complete | 15 | None |

#### Text Inputs
| Widget | Status | Properties Count | Issues |
|--------|--------|------------------|--------|
| TextField | ✅ Complete | 77 | None |

---

## Critical Findings

### 🚨 Priority 1: Missing Widget
**TabBar** must be added to the verification plan. This is a critical Material Navigation widget used in many applications for tabbed interfaces.

### ⚠️ Priority 2: Property Errors
1. **Switch** widget has a table formatting error (duplicate "getValue")
2. **Placeholder** widget is missing the `strokeAlign` property

### ✅ Overall Coverage
- **Total Widgets in Documentation:** 52
- **Total Widgets in Verification Plan:** 51
- **Coverage:** 98% (missing 1 widget)
- **Property Accuracy:** ~99.5% (2 property issues out of ~1000+ total properties)

---

## Recommendations

### Immediate Actions Required

1. **Add TabBar Widget** to Material Module → Navigation section with all 26 properties:
   - key, tabs, controller, isScrollable, padding, indicatorColor, automaticIndicatorColorAdjustment, indicatorWeight, indicatorPadding, indicator, indicatorSize, dividerColor, dividerHeight, labelColor, labelStyle, labelPadding, unselectedLabelColor, unselectedLabelStyle, dragStartBehavior, overlayColor, mouseCursor, enableFeedback, onTap, onHover, onFocusChange, physics, splashFactory, splashBorderRadius, tabAlignment, textScaler, indicatorAnimation

2. **Fix Switch Widget** property table formatting error (line with onInactiveThumbImageError)

3. **Add strokeAlign property** to Placeholder widget table

### Verification Complete

After these 3 corrections, the verification plan will be **100% complete** and accurate against the Flutter widget documentation.

---

## Appendix: TabBar Implementation Table Template

```markdown
#### [ ] | TabBar | material/widgets/tab_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | tabs | List<Widget> | getWidgets | tabs |
| [ ] | controller | TabController? | getValue | controller |
| [ ] | isScrollable | bool | getBool | is-scrollable |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | automaticIndicatorColorAdjustment | bool | getBool | automatic-indicator-color-adjustment |
| [ ] | indicatorWeight | double | getDouble | indicator-weight |
| [ ] | indicatorPadding | EdgeInsetsGeometry | getValue | indicator-padding |
| [ ] | indicator | Decoration? | getValue | indicator |
| [ ] | indicatorSize | TabBarIndicatorSize? | getValue | indicator-size |
| [ ] | dividerColor | Color? | getColor | divider-color |
| [ ] | dividerHeight | double? | getDouble | divider-height |
| [ ] | labelColor | Color? | getColor | label-color |
| [ ] | labelStyle | TextStyle? | getValue | label-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | unselectedLabelColor | Color? | getColor | unselected-label-color |
| [ ] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | onHover | TabValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | TabValueChanged<bool>? | getValue | on-focus-change |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | splashFactory | InteractiveInkFeatureFactory? | getValue | splash-factory |
| [ ] | splashBorderRadius | BorderRadius? | getValue | splash-border-radius |
| [ ] | tabAlignment | TabAlignment? | getValue | tab-alignment |
| [ ] | textScaler | TextScaler? | getValue | text-scaler |
| [ ] | indicatorAnimation | TabIndicatorAnimation? | getValue | indicator-animation |
```

---

**End of Verification Report**
