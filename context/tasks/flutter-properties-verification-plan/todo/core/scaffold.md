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
| [ ] | appBar | PreferredSizeWidget? | getValue | app-bar |
| [ ] | body | Widgeperties ad t? | getWidget | body |
| [ ] | floatingActionButton | Widget? | getWidget | floating-action-button |
| [ ] | floatingActionButtonLocation | FloatingActionButtonLocation? | getValue | floating-action-button-location |
| [ ] | floatingActionButtonAnimator | FloatingActionButtonAnimator? | getValue | floating-action-button-animator |
| [ ] | persistentFooterButtons | List<Widget>? | getWidgets | persistent-footer-buttons |
| [ ] | drawer | Widget? | getWidget | drawer |
| [ ] | endDrawer | Widget? | getWidget | end-drawer |
| [ ] | bottomNavigationBar | Widget? | getWidget | bottom-navigation-bar |
| [ ] | bottomSheet | Widget? | getWidget | bottom-sheet |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | resizeToAvoidBottomInset | bool? | getBool | resize-to-avoid-bottom-inset |
| [ ] | primary | bool | getBool | primary |
| [ ] | drawerDragStartBehavior | DragStartBehavior | getValue | drawer-drag-start-behavior |
| [ ] | extendBody | bool | getBool | extend-body |
| [ ] | extendBodyBehindAppBar | bool | getBool | extend-body-behind-app-bar |
| [ ] | drawerScrimColor | Color? | getColor | drawer-scrim-color |
| [ ] | drawerEdgeDragWidth | double? | getDouble | drawer-edge-drag-width |
| [ ] | drawerEnableOpenDragGesture | bool | getBool | drawer-enable-open-drag-gesture |
| [ ] | endDrawerEnableOpenDragGesture | bool | getBool | end-drawer-enable-open-drag-gesture |
| [ ] | restorationId | String? | getString | restoration-id |
