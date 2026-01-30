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
| [ ] | key | Key? | properties.key | - |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | automaticallyImplyLeading | bool | getBool | automatically-imply-leading |
| [ ] | title | Widget? | getWidget | title |
| [ ] | actions | List<Widget>? | getWidgets | actions |
| [ ] | automaticallyImplyActions | bool | getBool | automatically-imply-actions |
| [ ] | flexibleSpace | Widget? | getWidget | flexible-space |
| [ ] | bottom | PreferredSizeWidget? | getValue | bottom |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | scrolledUnderElevation | double? | getDouble | scrolled-under-elevation |
| [ ] | notificationPredicate | ScrollNotificationPredicate | getValue | notification-predicate |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | foregroundColor | Color? | getColor | foreground-color |
| [ ] | iconTheme | IconThemeData? | getValue | icon-theme |
| [ ] | actionsIconTheme | IconThemeData? | getValue | actions-icon-theme |
| [ ] | primary | bool | getBool | primary |
| [ ] | centerTitle | bool? | getBool | center-title |
| [ ] | excludeHeaderSemantics | bool | getBool | exclude-header-semantics |
| [ ] | titleSpacing | double? | getDouble | title-spacing |
| [ ] | toolbarOpacity | double | getDouble | toolbar-opacity |
| [ ] | bottomOpacity | double | getDouble | bottom-opacity |
| [ ] | toolbarHeight | double? | getDouble | toolbar-height |
| [ ] | leadingWidth | double? | getDouble | leading-width |
| [ ] | toolbarTextStyle | TextStyle? | getValue | toolbar-text-style |
| [ ] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [ ] | systemOverlayStyle | SystemUiOverlayStyle? | getValue | system-overlay-style |
| [ ] | forceMaterialTransparency | bool | getBool | force-material-transparency |
| [ ] | useDefaultSemanticsOrder | bool | getBool | use-default-semantics-order |
| [ ] | clipBehavior | Clip? | getValue | clip-behavior |
| [ ] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [ ] | animateColor | bool | getBool | animate-color |
