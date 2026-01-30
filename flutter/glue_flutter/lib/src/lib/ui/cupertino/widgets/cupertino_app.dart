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
      navigatorKey: properties.getValue('navigator-key'),
      home: properties.getWidget('home'),
      theme: properties.getValue('theme'),
      routes: properties.getValue('routes'),
      initialRoute: properties.getString('initial-route'),
      onGenerateRoute: properties.getValue('on-generate-route'),
      onGenerateInitialRoutes: properties.getValue(
        'on-generate-initial-routes',
      ),
      onUnknownRoute: properties.getValue('on-unknown-route'),
      navigatorObservers: properties.getValue('navigator-observers'),
      builder: properties.getValue('builder'),
      title: properties.getString('title') ?? 'Glue App',
      onGenerateTitle: properties.getValue('on-generate-title'),
      color: properties.getColor('color'),
      locale: properties.getValue('locale'),
      localizationsDelegates: properties.getValue('localizations-delegates'),
      localeListResolutionCallback: properties.getValue(
        'locale-list-resolution-callback',
      ),
      localeResolutionCallback: properties.getValue(
        'locale-resolution-callback',
      ),
      supportedLocales: properties.getValue('supported-locales'),
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
      shortcuts: properties.getValue('shortcuts'),
      actions: properties.getValue('actions'),
      restorationScopeId: properties.getString('restoration-scope-id'),
      scrollBehavior: properties.getValue('scroll-behavior'),
    );
    return IrNativeValue(Value(widget));
  });
}
