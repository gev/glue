- [x] AppBar

### Property Access Methods by Type
- **Direct Property Access**: `key`, `child`, `children`, `width`, `height`, `top`, `bottom`, `left`, `right`, `start`, `end`, `horizontal`, `vertical`
  - Access as: `properties.key`, `properties.child`, `properties.children`, etc.
- **bool**: `properties.getBool('property-name')`
- **Color**: `properties.getColor('property-name')`
- **double**: `properties.getDouble('property-name')`
- **int**: `properties.getInt('property-name')`
- **String**: `properties.getString('property-name')`
- **Widget**: `properties.getWidget('property-name')`
- **List\<Widge\>**: `properties.getWidgets('property-name')`
- **Complex/Custom/ScrollController/ValueChanged<T>**: `properties.getValue('property-name')`
- **VoidCallback**: `properties.getVoidCallback('property-name', runtime)` *(requires runtime wrapper)*

### Implementation Requirements
- Always include `key: properties.key` as first constructor parameter
- For widgets with VoidCallback properties, wrap constructor in runtime function:
  ```dart
  Eval<Ir> _createWidget(WidgetProperties properties) {
    return getRuntime().map((runtime) {
      final widget = Constructor(
        key: properties.key,
        onPressed: properties.getVoidCallback('on-pressed', runtime),
        // ... other properties
      );
      return IrNativeValue(Value(widget));
    });
  }
  ```

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | leading | Widget? | getWidget | leading |
| [x] | automaticallyImplyLeading | bool | getBool | automatically-imply-leading |
| [x] | title | Widget? | getWidget | title |
| [x] | actions | List<Widget>? | getWidgets | actions |
| [x] | automaticallyImplyActions | bool | getBool | automatically-imply-actions |
| [x] | flexibleSpace | Widget? | getWidget | flexible-space |
| [x] | bottom | PreferredSizeWidget? | getValue | bottom |
| [x] | elevation | double? | getDouble | elevation |
| [x] | scrolledUnderElevation | double? | getDouble | scrolled-under-elevation |
| [x] | notificationPredicate | ScrollNotificationPredicate | getValue | notification-predicate |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | foregroundColor | Color? | getColor | foreground-color |
| [x] | iconTheme | IconThemeData? | getValue | icon-theme |
| [x] | actionsIconTheme | IconThemeData? | getValue | actions-icon-theme |
| [x] | primary | bool | getBool | primary |
| [x] | centerTitle | bool? | getBool | center-title |
| [x] | excludeHeaderSemantics | bool | getBool | exclude-header-semantics |
| [x] | titleSpacing | double? | getDouble | title-spacing |
| [x] | toolbarOpacity | double | getDouble | toolbar-opacity |
| [x] | bottomOpacity | double | getDouble | bottom-opacity |
| [x] | toolbarHeight | double? | getDouble | toolbar-height |
| [x] | leadingWidth | double? | getDouble | leading-width |
| [x] | toolbarTextStyle | TextStyle? | getValue | toolbar-text-style |
| [x] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [x] | systemOverlayStyle | SystemUiOverlayStyle? | getValue | system-overlay-style |
| [x] | forceMaterialTransparency | bool | getBool | force-material-transparency |
| [x] | useDefaultSemanticsOrder | bool | getBool | use-default-semantics-order |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [x] | animateColor | bool | getBool | animate-color |
