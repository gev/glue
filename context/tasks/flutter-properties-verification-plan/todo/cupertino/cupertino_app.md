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
| [ ] | navigatorKey | GlobalKey<NavigatorState>? | getValue | navigator-key |
| [ ] | home | Widget? | getWidget | home |
| [ ] | theme | CupertinoThemeData? | getValue | theme |
| [ ] | routes | Map<String, WidgetBuilder> | getValue | routes |
| [ ] | initialRoute | String? | getString | initial-route |
| [ ] | onGenerateRoute | RouteFactory? | getValue | on-generate-route |
| [ ] | onGenerateInitialRoutes | InitialRouteListFactory? | getValue | on-generate-initial-routes |
| [ ] | onUnknownRoute | RouteFactory? | getValue | on-unknown-route |
| [ ] | navigatorObservers | List<NavigatorObserver> | getValue | navigator-observers |
| [ ] | builder | TransitionBuilder? | getValue | builder |
| [ ] | title | String | getString | title |
| [ ] | onGenerateTitle | GenerateAppTitle? | getValue | on-generate-title |
| [ ] | color | Color? | getColor | color |
| [ ] | locale | Locale? | getValue | locale |
| [ ] | localizationsDelegates | Iterable<LocalizationsDelegate<dynamic>>? | getValue | localizations-delegates |
| [ ] | localeListResolutionCallback | LocaleListResolutionCallback? | getValue | locale-list-resolution-callback |
| [ ] | localeResolutionCallback | LocaleResolutionCallback? | getValue | locale-resolution-callback |
| [ ] | supportedLocales | Iterable<Locale> | getValue | supported-locales |
| [ ] | showPerformanceOverlay | bool | getBool | show-performance-overlay |
| [ ] | checkerboardRasterCacheImages | bool | getBool | checkerboard-raster-cache-images |
| [ ] | checkerboardOffscreenLayers | bool | getBool | checkerboard-offscreen-layers |
| [ ] | showSemanticsDebugger | bool | getBool | show-semantics-debugger |
| [ ] | debugShowCheckedModeBanner | bool | getBool | debug-show-checked-mode-banner |
| [ ] | shortcuts | Map<LogicalKeySet, Intent>? | getValue | shortcuts |
| [ ] | actions | Map<Type, Action<Intent>>? | getValue | actions |
| [ ] | restorationScopeId | String? | getString | restoration-scope-id |
| [ ] | scrollBehavior | ScrollBehavior? | getValue | scroll-behavior |
