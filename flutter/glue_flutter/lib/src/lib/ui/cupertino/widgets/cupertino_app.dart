import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoApp widget function
/// Creates Flutter CupertinoApp from Glue expressions
/// Expects keyword arguments: :home, :theme, :routes, :title, etc.
final Ir cupertinoApp = IrNativeFunc(cupertinoAppImpl);

/// CupertinoApp implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoAppImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoApp(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoApp(WidgetProperties.empty()),
};

/// Create CupertinoApp widget from properties object
Eval<Ir> _createCupertinoApp(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoApp(
      key: properties.key,
      navigatorKey: properties.getValue<GlobalKey<NavigatorState>>(
        'navigator-key',
      ),
      home: properties.getWidget('home'),
      theme: properties.getValue<CupertinoThemeData>('theme'),
      routes: properties.getValue<Map<String, WidgetBuilder>>('routes') ?? {},
      initialRoute: properties.getString('initial-route'),
      onGenerateRoute: properties.getValue<RouteFactory>('on-generate-route'),
      onGenerateInitialRoutes: properties.getValue<InitialRouteListFactory>(
        'on-generate-initial-routes',
      ),
      onUnknownRoute: properties.getValue<RouteFactory>('on-unknown-route'),
      navigatorObservers:
          properties.getValue<List<NavigatorObserver>>('navigator-observers') ??
          [],
      builder: properties.getValue<TransitionBuilder>('builder'),
      title: properties.getString('title') ?? 'Glue App',
      onGenerateTitle: properties.getValue<GenerateAppTitle>(
        'on-generate-title',
      ),
      color: properties.getColor('color'),
      locale: properties.getValue<Locale>('locale'),
      localizationsDelegates: properties
          .getValue<List<LocalizationsDelegate<dynamic>>>(
            'localizations-delegates',
          ),
      localeListResolutionCallback: properties
          .getValue<LocaleListResolutionCallback>(
            'locale-list-resolution-callback',
          ),
      localeResolutionCallback: properties.getValue<LocaleResolutionCallback>(
        'locale-resolution-callback',
      ),
      supportedLocales:
          properties.getValue<Iterable<Locale>>('supported-locales') ??
          const <Locale>[Locale('en', 'US')],
      showPerformanceOverlay:
          properties.getBool('show-performance-overlay') ?? false,
      checkerboardRasterCacheImages:
          properties.getBool('checkerboard-raster-cache-images') ?? false,
      checkerboardOffscreenLayers:
          properties.getBool('checkerboard-offscreen-layers') ?? false,
      showSemanticsDebugger:
          properties.getBool('show-semantics-debugger') ?? false,
      debugShowCheckedModeBanner:
          properties.getBool('debug-show-checked-mode-banner') ?? false,
      shortcuts: properties.getValue<Map<LogicalKeySet, Intent>>('shortcuts'),
      actions: properties.getValue<Map<Type, Action<Intent>>>('actions'),
      restorationScopeId: properties.getString('restoration-scope-id'),
      scrollBehavior: properties.getValue<ScrollBehavior>('scroll-behavior'),
    );
    return IrNativeValue(Value(widget));
  });
}
