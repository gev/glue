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
| [x] | navigatorKey | GlobalKey<NavigatorState>? | getValue | navigator-key |
| [x] | home | Widget? | getWidget | home |
| [x] | theme | CupertinoThemeData? | getValue | theme |
| [x] | routes | Map<String, WidgetBuilder> | getValue | routes |
| [x] | initialRoute | String? | getString | initial-route |
| [x] | onGenerateRoute | RouteFactory? | getValue | on-generate-route |
| [x] | onGenerateInitialRoutes | InitialRouteListFactory? | getValue | on-generate-initial-routes |
| [x] | onUnknownRoute | RouteFactory? | getValue | on-unknown-route |
| [x] | navigatorObservers | List<NavigatorObserver> | getValue | navigator-observers |
| [x] | builder | TransitionBuilder? | getValue | builder |
| [x] | title | String | getString | title |
| [x] | onGenerateTitle | GenerateAppTitle? | getValue | on-generate-title |
| [x] | color | Color? | getColor | color |
| [x] | locale | Locale? | getValue | locale |
| [x] | localizationsDelegates | Iterable<LocalizationsDelegate<dynamic>>? | getValue | localizations-delegates |
| [x] | localeListResolutionCallback | LocaleListResolutionCallback? | getValue | locale-list-resolution-callback |
| [x] | localeResolutionCallback | LocaleResolutionCallback? | getValue | locale-resolution-callback |
| [x] | supportedLocales | Iterable<Locale> | getValue | supported-locales |
| [x] | showPerformanceOverlay | bool | getBool | show-performance-overlay |
| [x] | checkerboardRasterCacheImages | bool | getBool | checkerboard-raster-cache-images |
| [x] | checkerboardOffscreenLayers | bool | getBool | checkerboard-offscreen-layers |
| [x] | showSemanticsDebugger | bool | getBool | show-semantics-debugger |
| [x] | debugShowCheckedModeBanner | bool | getBool | debug-show-checked-mode-banner |
| [x] | shortcuts | Map<LogicalKeySet, Intent>? | getValue | shortcuts |
| [x] | actions | Map<Type, Action<Intent>>? | getValue | actions |
| [x] | restorationScopeId | String? | getString | restoration-scope-id |
| [x] | scrollBehavior | ScrollBehavior? | getValue | scroll-behavior |
