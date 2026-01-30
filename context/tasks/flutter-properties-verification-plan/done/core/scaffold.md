### Scaffold Widget Verification [x]

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
| [x] | appBar | PreferredSizeWidget? | getValue | app-bar |
| [x] | body | Widget? | getWidget | body |
| [x] | floatingActionButton | Widget? | getWidget | floating-action-button |
| [x] | floatingActionButtonLocation | FloatingActionButtonLocation? | getValue | floating-action-button-location |
| [x] | floatingActionButtonAnimator | FloatingActionButtonAnimator? | getValue | floating-action-button-animator |
| [x] | persistentFooterButtons | List<Widget>? | getWidgets | persistent-footer-buttons |
| [x] | drawer | Widget? | getWidget | drawer |
| [x] | endDrawer | Widget? | getWidget | end-drawer |
| [x] | bottomNavigationBar | Widget? | getWidget | bottom-navigation-bar |
| [x] | bottomSheet | Widget? | getWidget | bottom-sheet |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | resizeToAvoidBottomInset | bool? | getBool | resize-to-avoid-bottom-inset |
| [x] | primary | bool | getBool | primary |
| [x] | drawerDragStartBehavior | DragStartBehavior | getValue | drawer-drag-start-behavior |
| [x] | extendBody | bool | getBool | extend-body |
| [x] | extendBodyBehindAppBar | bool | getBool | extend-body-behind-app-bar |
| [x] | drawerScrimColor | Color? | getColor | drawer-scrim-color |
| [x] | drawerEdgeDragWidth | double? | getDouble | drawer-edge-drag-width |
| [x] | drawerEnableOpenDragGesture | bool | getBool | drawer-enable-open-drag-gesture |
| [x] | endDrawerEnableOpenDragGesture | bool | getBool | end-drawer-enable-open-drag-gesture |
| [x] | restorationId | String? | getString | restoration-id |
