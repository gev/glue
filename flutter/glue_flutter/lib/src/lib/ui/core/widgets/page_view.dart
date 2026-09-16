import 'package:flutter/gestures.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// PageView widget function
/// Creates Flutter PageView from Glue expressions
final Ir pageView = IrNativeFunc(pageViewImpl);

/// PageView implementation - takes properties object
Eval<Ir> pageViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPageView(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create PageView widget from properties
Eval<Ir> _createPageView(WidgetProperties properties) {
  // Обратите внимание: для controller и onPageChanged в зависимости от вашей архитектуры
  // может потребоваться специальный парсинг колбэков/контроллеров.
  // Здесь они указаны через getValue, если движок поддерживает их передачу.

  final pageViewWidget = PageView(
    key: properties.key,
    scrollDirection:
        properties.getValue<Axis>('scroll-direction') ?? Axis.horizontal,
    reverse: properties.getBool('reverse') ?? false,
    controller: properties.getValue<PageController>('controller'),
    physics: properties.getValue<ScrollPhysics>('physics'),
    pageSnapping: properties.getBool('page-snapping') ?? true,
    onPageChanged: properties.getValue<void Function(int)>('on-page-changed'),
    children: properties.children,
    dragStartBehavior:
        properties.getValue<DragStartBehavior>('drag-start-behavior') ??
        DragStartBehavior.start,
    allowImplicitScrolling:
        properties.getBool('allow-implicit-scrolling') ?? false,
    scrollCacheExtent: properties.getValue<ScrollCacheExtent>(
      'scroll-cache-extent',
    ),
    restorationId: properties.getString('restoration-id'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    hitTestBehavior:
        properties.getValue<HitTestBehavior>('hit-test-behavior') ??
        HitTestBehavior.opaque,
    scrollBehavior: properties.getValue<ScrollBehavior>('scroll-behavior'),
    padEnds: properties.getBool('pad-ends') ?? true,
  );

  return Eval.pure(IrNativeValue(Value(pageViewWidget)));
}
