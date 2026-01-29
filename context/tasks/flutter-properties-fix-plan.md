# Flutter UI Widget Properties Fix Plan

Based on [development rules](../development-rules.md#dart-code) and [Flutter guidelines](../development-rules.md#flutter-code), this plan implements type-safe widget property extraction using direct getters for common properties (key, child, children, width, height, etc.) and typed getter methods for others.

## Objective
Fix all 752 undefined getter/method errors in flutter/glue_flutter/lib/src/lib/ui/ by converting widget implementations to use proper WidgetProperties API, ensuring:

- Type safety (avoid `dynamic`/`Object`)
- Pattern matching over conditionals
- One widget per file
- Proper lifecycle management
- Consistent naming (PasacalCase widgets, camelCase variables, snake_case files)

## Rules and Guidelines
### Property Naming
- Use kebab-case for property names: `camelCase` → `'camel-case'`
- Examples:
  - `onChanged` → `'on-changed'`
  - `activeThumbColor` → `'active-thumb-color'`
  - `mainAxisAlignment` → `'main-axis-alignment'`

### Direct Getters (use as-is)
Use these getters directly without modification:
- `key`, `child`, `children`, `width`, `height`, `top`, `bottom`, `left`, `right`, `start`, `end`, `horizontal`, `vertical`

### Type-based Getter/Method Usage
Based on Flutter Widget property types:
- **Color**: `properties.getColor('property-name')`
- **double**: `properties.getDouble('property-name')` or direct getter if in the list above
- **int**: `properties.getInt('property-name')`
- **String**: `properties.getString('property-name')`
- **Widget**: `properties.getWidget('property-name')`
- **List<Widget>**: `properties.getWidgets('property-name')`
- **VoidCallback**: `properties.getVoidCallback('property-name', runtime)`
- **Other types**: `properties.getValue('property-name')`

### Key Property
- Add `key: properties.key` as the first parameter to all widget constructors

### Runtime Handling for Callbacks
- For widgets using `VoidCallback` properties, update the creation function to:
  ```dart
  Eval<Ir> _createWidget(WidgetProperties properties) {
    return getRuntime().map((runtime) {
      final widget = WidgetConstructor(
        key: properties.key,
        // ... other properties
        onChanged: properties.getVoidCallback('on-changed', runtime),
        // ...
      );
      return IrNativeValue(Value(widget));
    });
  }
  ```

## Task Breakdown

### Phase 1: Core Widgets (High Priority)
1. Fix core layout widgets:
   - core/widgets/grid_view.dart
   - core/widgets/icon.dart
   - core/widgets/image.dart
   - core/widgets/list_view.dart
   - core/widgets/padding.dart
   - core/widgets/placeholder.dart
   - core/widgets/row.dart
   - core/widgets/single_child_scroll_view.dart
   - core/widgets/sliver_grid.dart
   - core/widgets/sliver_list.dart
   - core/widgets/text.dart

### Phase 2: Material Widgets
1. Fix button widgets:
   - `button.dart`
   - `text_button.dart` (already has getRuntime pattern)
   - `elevated_button.dart`
   - `outlined_button.dart`
   - `floating_action_button.dart`
2. Fix form controls:
   - `checkbox.dart`
   - `switch.dart`
   - `text_field.dart`
   - `dropdown_button.dart`
3. Fix navigation and layout:
   - `app_bar.dart`
   - `drawer.dart`
   - `tab_bar.dart`
   - `tab_bar_view.dart`
4. Fix other material widgets:
   - material/widgets/action_chip.dart
   - material/widgets/badge.dart
   - material/widgets/bottom_app_bar.dart
   - material/widgets/bottom_navigation_bar.dart
   - material/widgets/bottom_sheet.dart
   - material/widgets/chip.dart
   - material/widgets/choice_chip.dart
   - material/widgets/circular_progress_indicator.dart
   - material/widgets/data_table.dart
   - material/widgets/drawer_header.dart
   - material/widgets/drawer.dart
   - material/widgets/elevated_button.dart
   - material/widgets/expansion_tile.dart
   - material/widgets/filled_button.dart
   - material/widgets/filter_chip.dart
   - material/widgets/icon_button.dart
   - material/widgets/input_chip.dart
   - material/widgets/linear_progress_indicator.dart
   - material/widgets/menu_anchor.dart
   - material/widgets/navigation_bar.dart
   - material/widgets/navigation_drawer.dart
   - material/widgets/navigation_rail.dart
   - material/widgets/outlined_button.dart
   - material/widgets/popup_menu_button.dart
   - material/widgets/radio.dart
   - material/widgets/refresh_indicator.dart
   - material/widgets/scaffold.dart
   - material/widgets/search_anchor.dart
   - material/widgets/search_bar.dart
   - material/widgets/segmented_button.dart
   - material/widgets/slider.dart
   - material/widgets/snack_bar.dart
   - material/widgets/stepper.dart
   - material/widgets/switch.dart
   - material/widgets/tab_bar_view.dart
   - material/widgets/tab_bar.dart
   - material/widgets/text_button.dart

### Phase 3: Cupertino Widgets
1. Fix cupertino widgets:
   - cupertino/widgets/cupertino_action_sheet.dart
   - cupertino/widgets/cupertino_activity_indicator.dart
   - cupertino/widgets/cupertino_alert_dialog.dart
   - cupertino/widgets/cupertino_button.dart
   - cupertino/widgets/cupertino_checkbox.dart
   - cupertino/widgets/cupertino_context_menu.dart
   - cupertino/widgets/cupertino_date_picker.dart
   - cupertino/widgets/cupertino_navigation_bar.dart
   - cupertino/widgets/cupertino_page_scaffold.dart
   - cupertino/widgets/cupertino_picker.dart
   - cupertino/widgets/cupertino_scrollbar.dart
   - cupertino/widgets/cupertino_search_text_field.dart
   - cupertino/widgets/cupertino_segmented_control.dart
   - cupertino/widgets/cupertino_slider.dart
   - cupertino/widgets/cupertino_switch.dart
   - cupertino/widgets/cupertino_tab_bar.dart
   - cupertino/widgets/cupertino_text_field.dart
   - cupertino/widgets/cupertino_timer_picker.dart

### Phase 4: Style Modules
1. Fix padding and alignment styles in `core/styles/` if any errors

### Phase 5: Verification
1. Run `flutter analyze` in `flutter/glue_flutter/`
2. Ensure 0 issues reported
3. Test basic functionality if possible

## Common Patterns to Fix

### Pattern 1: Direct undefined property
```dart
// BEFORE
value: properties.value,

// AFTER
value: properties.getValue('value'),
```

### Pattern 2: Color property
```dart
// BEFORE
color: properties.color,

// AFTER
color: properties.getColor('color'),
```

### Pattern 3: Callback property
```dart
// BEFORE
onPressed: properties.onPressed,

// AFTER (with runtime wrapper)
return getRuntime().map((runtime) => Widget(
  onPressed: properties.getVoidCallback('on-pressed', runtime),
  // ...
));
```

### Pattern 4: Dimension/double
```dart
// BEFORE
width: properties.widthValue,

// AFTER (if not in direct getters list)
width: properties.getDouble('width-value'),
```

## Expected Outcome
- All 752 analytical issues resolved
- Widgets use correct WidgetProperties API
- Properties follow kebab-case naming convention
- All widgets include key parameter
- No runtime or compile errors
