

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

## Verification Tables by Module

### Core Module: flutter/glue_flutter/lib/src/lib/ui/core/

#### [x] | AppBar | core/widgets/app_bar.dart
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

#### [x] | Column | core/widgets/column.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | mainAxisAlignment | MainAxisAlignment | getValue | main-axis-alignment |
| [x] | mainAxisSize | MainAxisSize | getValue | main-axis-size |
| [x] | crossAxisAlignment | CrossAxisAlignment | getValue | cross-axis-alignment |
| [x] | textDirection | TextDirection? | getValue | text-direction |
| [x] | verticalDirection | VerticalDirection | getValue | vertical-direction |
| [x] | textBaseline | TextBaseline? | getValue | text-baseline |
| [x] | children | List<Widget> | properties.children | - |

#### [x] | Container | core/widgets/container.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | alignment | AlignmentGeometry? | getValue | alignment |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | color | Color? | getColor | color |
| [x] | decoration | Decoration? | getValue | decoration |
| [x] | foregroundDecoration | Decoration? | getValue | foreground-decoration |
| [x] | width | double? | properties.width | - |
| [x] | height | double? | properties.height | - |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | margin | EdgeInsetsGeometry? | getValue | margin |
| [x] | transform | Matrix4? | getValue | transform |
| [x] | transformAlignment | AlignmentGeometry? | getValue | transform-alignment |
| [x] | child | Widget? | properties.child | - |
| [x] | clipBehavior | Clip | getValue | clip-behavior |

#### [x] | ElevatedButton | core/widgets/elevated_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | style | ButtonStyle? | getValue | style |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | child | Widget | properties.child | - |

#### [x] | FlutterLogo | core/widgets/flutter_logo.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | size | double | getDouble | size |
| [x] | style | FlutterLogoStyle | getValue | style |
| [x] | textColor | Color? | getColor | text-color |
| [x] | colors | Color? | getColor | colors |
| [x] | duration | Duration | getValue | duration |
| [x] | curve | Curve | getValue | curve |

#### [x] | Icon | core/widgets/icon.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | icon | IconData | getValue | icon |
| [x] | size | double? | getDouble | size |
| [x] | color | Color? | getColor | color |
| [x] | semanticLabel | String? | getString | semantic-label |
| [x] | textDirection | TextDirection? | getValue | text-direction |

#### [x] | Image | core/widgets/image.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | image | ImageProvider<Object> | getValue | image |
| [x] | width | double? | properties.width | - |
| [x] | height | double? | properties.height | - |
| [x] | color | Color? | getColor | color |
| [x] | colorBlendMode | BlendMode? | getValue | color-blend-mode |
| [x] | fit | BoxFit? | getValue | fit |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | repeat | ImageRepeat | getValue | repeat |
| [x] | centerSlice | Rect? | getValue | center-slice |
| [x] | matchTextDirection | bool | getBool | match-text-direction |
| [x] | gaplessPlayback | bool | getBool | gapless-playback |
| [x] | semanticLabel | String? | getString | semantic-label |
| [x] | excludeFromSemantics | bool | getBool | exclude-from-semantics |
| [x] | filterQuality | FilterQuality | getValue | filter-quality |
| [x] | cacheWidth | int? | getInt | cache-width |
| [x] | cacheHeight | int? | getInt | cache-height |

#### [x] | Placeholder | core/widgets/placeholder.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | fallbackWidth | double | getDouble | fallback-width |
| [x] | fallbackHeight | double | getDouble | fallback-height |
| [x] | color | Color? | getColor | color |
| [x] | strokeAlign | StrokeAlign | getValue | stroke-align |
| [x] | strokeWidth | double | getDouble | stroke-width |

#### [x] | Row | core/widgets/row.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | mainAxisAlignment | MainAxisAlignment | getValue | main-axis-alignment |
| [x] | mainAxisSize | MainAxisSize | getValue | main-axis-size |
| [x] | crossAxisAlignment | CrossAxisAlignment | getValue | cross-axis-alignment |
| [x] | textDirection | TextDirection? | getValue | text-direction |
| [x] | verticalDirection | VerticalDirection | getValue | vertical-direction |
| [x] | textBaseline | TextBaseline? | getValue | text-baseline |
| [x] | children | List<Widget> | properties.children | - |

#### [x] | Scaffold | core/widgets/scaffold.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | appBar | PreferredSizeWidget? | getValue | app-bar |
| [x] | body | Widgeperties ad t? | getWidget | body |
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

#### [x] | Text | core/widgets/text.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | data | String | - | data |
| [x] | style | TextStyle? | getValue | style |
| [x] | strutStyle | StrutStyle? | getValue | strut-style |
| [x] | textAlign | TextAlign? | getValue | text-align |
| [x] | textDirection | TextDirection? | getValue | text-direction |
| [x] | locale | Locale? | getValue | locale |
| [x] | softWrap | bool? | getBool | soft-wrap |
| [x] | overflow | TextOverflow? | getValue | overflow |
| [x] | textScaleFactor | double? | getDouble | text-scale-factor |
| [x] | maxLines | int? | getInt | max-lines |
| [x] | semanticsLabel | String? | getString | semantics-label |
| [x] | textWidthBasis | TextWidthBasis? | getValue | text-width-basis |
| [x] | textHeightBehavior | TextHeightBehavior? | getValue | text-height-behavior |

#### [x] | Center | core/widgets/center.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget? | properties.child | - |

#### [x] | Padding | core/widgets/padding.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | child | Widget? | properties.child | - |

#### [x] | ListView | core/widgets/list_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | scrollDirection | Axis | getValue | scroll-direction |
| [x] | reverse | bool | getValue | reverse |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | primary | bool? | getValue | primary |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | shrinkWrap | bool | getValue | shrink-wrap |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | itemExtent | double? | getDouble | item-extent |
| [x] | prototypeItem | Widget? | getWidget | prototype-item |
| [x] | addAutomaticKeepAlives | bool | getValue | add-automatic-keep-alives |
| [x] | addRepaintBoundaries | bool | getValue | add-repaint-boundaries |
| [x] | addSemanticIndexes | bool | getValue | add-semantic-indexes |
| [x] | cacheExtent | double? | getDouble | cache-extent |
| [x] | children | List<Widget> | properties.children | - |
| [x] | semanticChildCount | int? | getInt | semantic-child-count |
| [x] | clipBehavior | Clip | getValue | clip-behavior |

#### [x] | GridView | core/widgets/grid_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | scrollDirection | Axis | getValue | scroll-direction |
| [x] | reverse | bool | getValue | reverse |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | primary | bool? | getValue | primary |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | shrinkWrap | bool | getValue | shrink-wrap |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | gridDelegate | SliverGridDelegate | getValue | grid-delegate |
| [x] | addAutomaticKeepAlives | bool | getValue | add-automatic-keep-alives |
| [x] | addRepaintBoundaries | bool | getValue | add-repaint-boundaries |
| [x] | addSemanticIndexes | bool | getValue | add-semantic-indexes |
| [x] | cacheExtent | double? | getDouble | cache-extent |
| [x] | children | List<Widget> | properties.children | - |
| [x] | semanticChildCount | int? | getInt | semantic-child-count |
| [x] | clipBehavior | Clip | getValue | clip-behavior |

#### [x] | SingleChildScrollView | core/widgets/single_child_scroll_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | scrollDirection | Axis | getValue | scroll-direction |
| [x] | reverse | bool | getValue | reverse |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | primary | bool? | getValue | primary |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [x] | child | Widget? | properties.child | - |

#### [x] | CustomScrollView | core/widgets/custom_scroll_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | scrollDirection | Axis | getValue | scroll-direction |
| [x] | reverse | bool | getBool | reverse |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | primary | bool? | getBool | primary |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | shrinkWrap | bool | getBool | shrink-wrap |
| [x] | center | Key? | getKey | center |
| [x] | anchor | double | getDouble | anchor |
| [x] | cacheExtent | double? | getDouble | cache-extent |
| [x] | slivers | List<Widget> | getWidgets | slivers |
| [x] | semanticChildCount | int? | getInt | semantic-child-count |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | clipBehavior | Clip | getValue | clip-behavior |

#### [x] | SliverGrid | core/widgets/sliver_grid.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | delegate | SliverChildDelegate | getValue | sliver-grid-delegate |
| [x] | gridDelegate | SliverGridDelegate | getValue | sliver-grid-grid-delegate |

#### [x] | SliverList | core/widgets/sliver_list.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | delegate | SliverChildDelegate | getValue | sliver-list-delegate |

### Cupertino Module: flutter/glue_flutter/lib/src/lib/ui/cupertino/

#### [x] | CupertinoActionSheet | cupertino/widgets/cupertino_action_sheet.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | title | Widget? | getWidget | title |
| [x] | message | Widget? | getWidget | message |
| [x] | actions | List<Widget>? | getWidgets | actions |
| [x] | messageScrollController | ScrollController? | getValue | message-scroll-controller |
| [x] | actionScrollController | ScrollController? | getValue | action-scroll-controller |
| [x] | cancelButton | Widget? | getWidget | cancel-button |

#### [x] | CupertinoActivityIndicator | cupertino/widgets/cupertino_activity_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | color | Color? | getColor | color |
| [x] | animating | bool | getBool | animating |
| [x] | radius | double | getDouble | radius |

#### [x] | CupertinoAlertDialog | cupertino/widgets/cupertino_alert_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | title | Widget? | getWidget | title |
| [x] | content | Widget? | getWidget | content |
| [x] | actions | List<Widget> | getWidgets | actions |
| [x] | scrollController | ScrollController? | getValue | scroll-controller |
| [x] | actionScrollController | ScrollController? | getValue | action-scroll-controller |
| [x] | insetAnimationDuration | Duration | getValue | inset-animation-duration |
| [x] | insetAnimationCurve | Curve | getValue | inset-animation-curve |

#### [x] | CupertinoApp | cupertino/widgets/cupertino_app.dart
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

#### [x] | CupertinoButton | cupertino/widgets/cupertino_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget | properties.child | - |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | color | Color? | getColor | color |
| [x] | disabledColor | Color | getColor | disabled-color |
| [x] | minSize | double | getDouble | min-size |
| [x] | pressedOpacity | double | getDouble | pressed-opacity |
| [x] | borderRadius | BorderRadius | getValue | border-radius |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |

#### [x] | CupertinoCheckbox | cupertino/widgets/cupertino_checkbox.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | bool? | getBool | value |
| [x] | tristate | bool | getBool | tristate |
| [x] | onChanged | ValueChanged<bool?>? | getValue | on-changed |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | inactiveColor | Color? | getColor | inactive-color |
| [x] | checkColor | Color? | getColor | check-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | autofocus | bool | getBool | autofocus |

#### [x] | CupertinoContextMenu | cupertino/widgets/cupertino_context_menu.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | actions | List<Widget> | getWidgets | actions |
| [x] | child | Widget | properties.child | - |
| [x] | previewBuilder | WidgetBuilder? | getValue | preview-builder |

#### [x] | CupertinoDatePicker | cupertino/widgets/cupertino_date_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | mode | CupertinoDatePickerMode | getValue | mode |
| [x] | onDateTimeChanged | ValueChanged<DateTime> | getValue | on-date-time-changed |
| [x] | initialDateTime | DateTime? | getValue | initial-date-time |
| [x] | minimumDate | DateTime? | getValue | minimum-date |
| [x] | maximumDate | DateTime? | getValue | maximum-date |
| [x] | minimumYear | int | getInt | minimum-year |
| [x] | maximumYear | int? | getInt | maximum-year |
| [x] | minuteInterval | int | getInt | minute-interval |
| [x] | use24hFormat | bool | getBool | use24h-format |
| [x] | dateOrder | DatePickerDateOrder? | getValue | date-order |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | showDayOfWeek | bool | getBool | show-day-of-week |
| [x] | itemExtent | double | getDouble | item-extent |

#### [x] | CupertinoNavigationBar | cupertino/widgets/cupertino_navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | leading | Widget? | getWidget | leading |
| [x] | automaticallyImplyLeading | bool | getBool | automatically-imply-leading |
| [x] | automaticallyImplyMiddle | bool | getBool | automatically-imply-middle |
| [x] | previousPageTitle | String? | getString | previous-page-title |
| [x] | middle | Widget? | getWidget | middle |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | border | Border? | getValue | border |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | brightness | Brightness? | getValue | brightness |
| [x] | padding | EdgeInsetsDirectional? | getValue | padding |
| [x] | transitionBetweenRoutes | bool | getBool | transition-between-routes |
| [x] | heroTag | Object | getValue | hero-tag |

#### [x] | CupertinoPageScaffold | cupertino/widgets/cupertino_page_scaffold.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | navigationBar | ObstructingPreferredSizeWidget? | getValue | navigation-bar |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | resizeToAvoidBottomInset | bool | getBool | resize-to-avoid-bottom-inset |
| [x] | child | Widget | properties.child | - |

#### [x] | CupertinoPicker | cupertino/widgets/cupertino_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | diameterRatio | double | getDouble | diameter-ratio |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | offAxisFraction | double | getDouble | off-axis-fraction |
| [x] | useMagnifier | bool | getBool | use-magnifier |
| [x] | magnification | double | getDouble | magnification |
| [x] | scrollController | FixedExtentScrollController? | getValue | scroll-controller |
| [x] | squeeze | double | getDouble | squeeze |
| [x] | itemExtent | double | getDouble | item-extent |
| [x] | onSelectedItemChanged | ValueChanged<int>? | getValue | on-selected-item-changed |
| [x] | children | List<Widget> | properties.children | - |
| [x] | selectionOverlay | Widget | getValue | selection-overlay |

#### [x] | CupertinoScrollbar | cupertino/widgets/cupertino_scrollbar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget | properties.child | - |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | thumbVisibility | bool? | getBool | thumb-visibility |
| [x] | thickness | double | getDouble | thickness |
| [x] | thicknessWhileDragging | double | getDouble | thickness-while-dragging |
| [x] | radius | Radius | getValue | radius |
| [x] | radiusWhileDragging | Radius | getValue | radius-while-dragging |
| [x] | notificationPredicate | ScrollNotificationPredicate? | getValue | notification-predicate |

#### [x] | CupertinoSearchTextField | cupertino/widgets/cupertino_search_text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | controller | TextEditingController? | getValue | controller |
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | style | TextStyle? | getValue | style |
| [x] | placeholder | String? | getString | placeholder |
| [x] | placeholderStyle | TextStyle? | getValue | placeholder-style |
| [x] | decoration | BoxDecoration? | getValue | decoration |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | borderRadius | BorderRadius? | getValue | border-radius |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | itemColor | Color? | getColor | item-color |
| [x] | itemSize | double? | getDouble | item-size |
| [x] | prefixIcon | Widget? | getWidget | prefix-icon |
| [x] | prefixMode | OverlayVisibilityMode | getValue | prefix-mode |
| [x] | suffixIcon | Widget? | getWidget | suffix-icon |
| [x] | suffixMode | OverlayVisibilityMode | getValue | suffix-mode |
| [x] | onSuffixTap | VoidCallback? | getVoidCallback | on-suffix-tap |
| [x] | enabled | bool? | getBool | enabled |
| [x] | autocorrect | bool | getBool | autocorrect |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |

#### [x] | CupertinoSegmentedControl | cupertino/widgets/cupertino_segmented_control.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | children | Map<T, Widget> | getValue | children |
| [x] | onValueChanged | ValueChanged<T>? | getValue | on-value-changed |
| [x] | groupValue | T? | getValue | group-value |
| [x] | unselectedColor | Color | getColor | unselected-color |
| [x] | selectedColor | Color | getColor | selected-color |
| [x] | borderColor | Color | getColor | border-color |
| [x] | pressedColor | Color? | getColor | pressed-color |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |

#### [x] | CupertinoSlider | cupertino/widgets/cupertino_slider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | double | getDouble | value |
| [x] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [x] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [x] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [x] | min | double | getDouble | min |
| [x] | max | double | getDouble | max |
| [x] | divisions | int? | getInt | divisions |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | thumbColor | Color | getColor | thumb-color |

#### [x] | CupertinoSlidingSegmentedControl | cupertino/widgets/cupertino_sliding_segmented_control.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | children | Map<T, Widget> | getValue | children |
| [x] | onValueChanged | ValueChanged<T?>? | getValue | on-value-changed |
| [x] | groupValue | T? | getValue | group-value |
| [x] | thumbColor | Color | getColor | thumb-color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |

#### [x] | CupertinoSwitch | cupertino/widgets/cupertino_switch.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | bool | getBool | value |
| [x] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | trackColor | Color? | getColor | track-color |
| [x] | thumbColor | Color? | getColor | thumb-color |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |

#### [x] | CupertinoTabBar | cupertino/widgets/cupertino_tab_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | items | List<BottomNavigationBarItem> | getValue | items |
| [x] | onTap | ValueChanged<int>? | getValue | on-tap |
| [x] | currentIndex | int | getInt | current-index |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | inactiveColor | Color | getColor | inactive-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | border | Border? | getValue | border |

#### [x] | CupertinoTabScaffold | cupertino/widgets/cupertino_tab_scaffold.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | tabBar | CupertinoTabBar | getValue | tab-bar |
| [x] | tabBuilder | IndexedWidgetBuilder | getValue | tab-builder |
| [x] | controller | CupertinoTabController? | getValue | controller |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | resizeToAvoidBottomInset | bool | getBool | resize-to-avoid-bottom-inset |
| [x] | restorationId | String? | getString | restoration-id |

#### [x] | CupertinoTextField | cupertino/widgets/cupertino_text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | controller | TextEditingController? | getValue | controller |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | decoration | BoxDecoration | getValue | decoration |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | placeholder | String? | getString | placeholder |
| [x] | placeholderStyle | TextStyle | getValue | placeholder-style |
| [x] | prefix | Widget? | getWidget | prefix |
| [x] | prefixMode | OverlayVisibilityMode | getValue | prefix-mode |
| [x] | suffix | Widget? | getWidget | suffix |
| [x] | suffixMode | OverlayVisibilityMode | getValue | suffix-mode |
| [x] | clearButtonMode | OverlayVisibilityMode | getValue | clear-button-mode |
| [x] | keyboardType | TextInputType? | getValue | keyboard-type |
| [x] | textInputAction | TextInputAction? | getValue | text-input-action |
| [x] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [x] | style | TextStyle? | getValue | style |
| [x] | strutStyle | StrutStyle? | getValue | strut-style |
| [x] | textAlign | TextAlign | getValue | text-align |
| [x] | textAlignVertical | TextAlignVertical? | getValue | text-align-vertical |
| [x] | readOnly | bool | getBool | read-only |
| [x] | showCursor | bool? | getBool | show-cursor |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | obscuringCharacter | String | getString | obscuring-character |
| [x] | obscureText | bool | getBool | obscure-text |
| [x] | autocorrect | bool | getBool | autocorrect |
| [x] | smartDashesType | SmartDashesType? | getValue | smart-dashes-type |
| [x] | smartQuotesType | SmartQuotesType? | getValue | smart-quotes-type |
| [x] | enableSuggestions | bool | getBool | enable-suggestions |
| [x] | maxLines | int? | getInt | max-lines |
| [x] | minLines | int? | getInt | min-lines |
| [x] | expands | bool | getBool | expands |
| [x] | maxLength | int? | getInt | max-length |
| [x] | maxLengthEnforcement | MaxLengthEnforcement? | getValue | max-length-enforcement |
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onEditingComplete | VoidCallback? | getVoidCallback | on-editing-complete |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | inputFormatters | List<TextInputFormatter>? | getValue | input-formatters |
| [x] | enabled | bool? | getBool | enabled |
| [x] | cursorWidth | double | getDouble | cursor-width |
| [x] | cursorHeight | double? | getDouble | cursor-height |
| [x] | cursorRadius | Radius | getValue | cursor-radius |
| [x] | cursorColor | Color? | getColor | cursor-color |
| [x] | keyboardAppearance | Brightness? | getValue | keyboard-appearance |
| [x] | scrollPadding | EdgeInsets | getValue | scroll-padding |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | enableInteractiveSelection | bool | getBool | enable-interactive-selection |
| [x] | selectionControls | TextSelectionControls? | getValue | selection-controls |
| [x] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [x] | scrollController | ScrollController? | getValue | scroll-controller |
| [x] | scrollPhysics | ScrollPhysics? | getValue | scroll-physics |
| [x] | autofillHints | Iterable<String>? | getValue | autofill-hints |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | scribbleEnabled | bool | getBool | scribble-enabled |
| [x] | enableIMEPersonalizedLearning | bool | getBool | enable-ime-personalized-learning |

#### [x] | CupertinoTimerPicker | cupertino/widgets/cupertino_timer_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | mode | CupertinoTimerPickerMode | getValue | mode |
| [x] | initialTimerDuration | Duration | getValue | initial-timer-duration |
| [x] | minuteInterval | int | getInt | minute-interval |
| [x] | secondInterval | int | getInt | second-interval |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | itemExtent | double | getDouble | item-extent |
| [x] | onTimerDurationChanged | ValueChanged<Duration> | getValue | on-timer-duration-changed |

### Material Module: flutter/glue_flutter/lib/src/lib/ui/material/

#### [x] | ElevatedButton | material/widgets/elevated_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | style | ButtonStyle? | getValue | style |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | child | Widget | properties.child | - |

#### [x] | FilledButton | material/widgets/filled_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | style | ButtonStyle? | getValue | style |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | child | Widget | properties.child | - |

#### [x] | FloatingActionButton | material/widgets/floating_action_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget? | properties.child | - |
| [x] | tooltip | String? | getString | tooltip |
| [x] | foregroundColor | Color? | getColor | foreground-color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | heroTag | Object? | getValue | hero-tag |
| [x] | elevation | double? | getDouble | elevation |
| [x] | focusElevation | double? | getDouble | focus-elevation |
| [x] | hoverElevation | double? | getDouble | hover-elevation |
| [x] | highlightElevation | double? | getDouble | highlight-elevation |
| [x] | disabledElevation | double? | getDouble | disabled-elevation |
| [x] | mini | bool? | getBool | mini |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | isExtended | bool | getBool | is-extended |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | alignment | Alignment? | getValue | alignment |
| [x] | offset | Offset? | getValue | offset |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | restorationId | String? | getString | restoration-id |

#### [x] | IconButton | material/widgets/icon_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | icon | Widget? | properties.child | - |
| [x] | color | Color? | getColor | color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | highlightColor | Color? | getColor | highlight-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | tooltip | String? | getString | tooltip |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | mini | bool? | getBool | mini |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | restorationId | String? | getString | restoration-id |

#### [x] | OutlinedButton | material/widgets/outlined_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | style | ButtonStyle? | getValue | style |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | child | Widget | properties.child | - |

#### [x] | SegmentedButton | material/widgets/segmented_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | selected | Set<T> | getValue | selected |
| [x] | segments | List<Widget> | getWidgets | segments |
| [x] | onSelectionChanged | ValueChanged<Set<T>> | getValue | on-selection-changed |
| [x] | multiSelectionEnabledFor | Set<T>? | getValue | multi-selection-enabled-for |
| [x] | showSelectedIcon | bool? | getBool | show-selected-icon |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | unselectedColor | Color? | getColor | unselected-color |
| [x] | selectedColor | Color? | getColor | selected-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |

#### [x] | TextButton | material/widgets/text_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | style | ButtonStyle? | getValue | style |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | child | Widget | properties.child | - |

#### [x] | Badge | material/widgets/badge.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget | properties.child | - |
| [x] | label | Widget? | getWidget | label |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | textColor | Color? | getColor | text-color |
| [x] | textStyle | TextStyle? | getValue | text-style |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | alignment | AlignmentGeometry? | getValue | alignment |
| [x] | isLabelVisible | bool? | getBool | is-label-visible |
| [x] | largeSize | bool? | getBool | large-size |
| [x] | offset | Offset? | getValue | offset |
| [x] | showBadge | bool? | getBool | show-badge |

#### [x] | LinearProgressIndicator | material/widgets/linear_progress_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | double? | getDouble | value |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | color | Color? | getColor | color |
| [x] | valueColor | Animation<Color>? | getValue | value-color |
| [x] | minHeight | double? | getDouble | min-height |
| [x] | semanticsLabel | String? | getString | semantics-label |
| [x] | semanticsValue | String? | getString | semantics-value |

#### [x] | SnackBar | material/widgets/snack_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | content | Widget | properties.child | - |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | margin | EdgeInsetsGeometry? | getValue | margin |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | width | double? | properties.width | - |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | behavior | SnackBarBehavior? | getValue | behavior |
| [x] | action | SnackBarAction? | getValue | action |
| [x] | duration | Duration? | getValue | duration |
| [x] | animation | Animation<double>? | getValue | animation |
| [x] | onVisible | VoidCallback? | getVoidCallback | on-visible |
| [x] | dismissDirection | DismissDirection? | getValue | dismiss-direction |
| [x] | clipBehavior | Clip | getValue | clip-behavior |

#### [x] | AlertDialog | material/widgets/alert_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | icon | Widget? | getWidget | icon |
| [x] | iconPadding | EdgeInsetsGeometry? | getValue | icon-padding |
| [x] | iconColor | Color? | getColor | icon-color |
| [x] | title | Widget? | getWidget | title |
| [x] | titlePadding | EdgeInsetsGeometry? | getValue | title-padding |
| [x] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [x] | content | Widget? | getWidget | content |
| [x] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [x] | contentTextStyle | TextStyle? | getValue | content-text-style |
| [x] | actions | List<Widget>? | getWidgets | actions |
| [x] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [x] | actionsAlignment | MainAxisAlignment? | getValue | actions-alignment |
| [x] | actionsOverflowAlignment | OverflowBarAlignment? | getValue | actions-overflow-alignment |
| [x] | actionsOverflowDirection | VerticalDirection? | getValue | actions-overflow-direction |
| [x] | actionsOverflowButtonSpacing | double? | getDouble | actions-overflow-button-spacing |
| [x] | buttonPadding | EdgeInsetsGeometry? | getValue | button-padding |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | semanticLabel | String? | getString | semantic-label |
| [x] | insetPadding | EdgeInsets? | getValue | inset-padding |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | alignment | AlignmentGeometry? | getValue | alignment |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | scrollable | bool | getBool | scrollable |

#### [x] | BottomSheet | material/widgets/bottom_sheet.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | animationController | AnimationController? | getValue | animation-controller |
| [x] | enableDrag | bool | getBool | enable-drag |
| [x] | showDragHandle | bool? | getBool | show-drag-handle |
| [x] | dragHandleColor | Color? | getColor | drag-handle-color |
| [x] | dragHandleSize | Size? | getValue | drag-handle-size |
| [x] | onDragStart | BottomSheetDragStartHandler? | getValue | on-drag-start |
| [x] | onDragEnd | BottomSheetDragEndHandler? | getValue | on-drag-end |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | onClosing | VoidCallback | getVoidCallback | on-closing |
| [x] | builder | WidgetBuilder | getValue | builder |

#### [x] | Card | material/widgets/card.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | color | Color? | getColor | color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | borderOnForeground | bool | getBool | border-on-foreground |
| [x] | margin | EdgeInsetsGeometry? | getValue | margin |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | child | Widget? | properties.child | - |
| [x] | semanticContainer | bool | getBool | semantic-container |

#### [x] | Divider | material/widgets/divider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | height | double? | getDouble | height |
| [x] | thickness | double? | getDouble | thickness |
| [x] | indent | double? | getDouble | indent |
| [x] | endIndent | double? | getDouble | end-indent |
| [x] | color | Color? | getColor | color |
| [x] | radius | BorderRadiusGeometry? | getValue | radius |

#### [x] | ListTile | material/widgets/list_tile.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | leading | Widget? | getWidget | leading |
| [x] | title | Widget? | getWidget | title |
| [x] | subtitle | Widget? | getWidget | subtitle |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | isThreeLine | bool? | getBool | is-three-line |
| [x] | dense | bool? | getBool | dense |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | style | ListTileStyle? | getValue | style |
| [x] | selectedColor | Color? | getColor | selected-color |
| [x] | iconColor | Color? | getColor | icon-color |
| [x] | textColor | Color? | getColor | text-color |
| [x] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [x] | subtitleTextStyle | TextStyle? | getValue | subtitle-text-style |
| [x] | leadingAndTrailingTextStyle | TextStyle? | getValue | leading-and-trailing-text-style |
| [x] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [x] | enabled | bool | getBool | enabled |
| [x] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [x] | onLongPress | GestureLongPressCallback? | getValue | on-long-press |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | selected | bool | getBool | selected |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | tileColor | Color? | getColor | tile-color |
| [x] | selectedTileColor | Color? | getColor | selected-tile-color |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | horizontalTitleGap | double? | getDouble | horizontal-title-gap |
| [x] | minVerticalPadding | double? | getDouble | min-vertical-padding |
| [x] | minLeadingWidth | double? | getDouble | min-leading-width |
| [x] | minTileHeight | double? | getDouble | min-tile-height |
| [x] | titleAlignment | ListTileTitleAlignment? | getValue | title-alignment |
| [x] | internalAddSemanticForOnTap | bool | getBool | internal-add-semantic-for-on-tap |
| [x] | statesController | MaterialStatesController? | getValue | states-controller |

#### [x] | BottomAppBar | material/widgets/bottom_app_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | color | Color? | getColor | color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shape | NotchedShape? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | notchMargin | double | getDouble | notch-margin |
| [x] | child | Widget? | properties.child | - |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | height | double? | getDouble | height |

#### [x] | NavigationBar | material/widgets/navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | animationDuration | Duration? | getValue | animation-duration |
| [x] | selectedIndex | int | getInt | selected-index |
| [x] | destinations | List<Widget> | getWidgets | destinations |
| [x] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [x] | height | double? | getDouble | height |
| [x] | labelBehavior | NavigationDestinationLabelBehavior? | getValue | label-behavior |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | labelTextStyle | WidgetStateProperty<TextStyle?>? | getValue | label-text-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | maintainBottomViewPadding | bool | getBool | maintain-bottom-view-padding |

#### [x] | NavigationDrawer | material/widgets/navigation_drawer.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | children | List<Widget> | properties.children | - |
| [x] | header | Widget? | getWidget | header |
| [x] | footer | Widget? | getWidget | footer |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [x] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [x] | selectedIndex | int? | getInt | selected-index |
| [x] | tilePadding | EdgeInsetsGeometry | getValue | tile-padding |

#### [x] | NavigationRail | material/widgets/navigation_rail.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | extended | bool | getBool | extended |
| [x] | leading | Widget? | getWidget | leading |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | destinations | List<NavigationRailDestination> | getValue | destinations |
| [x] | selectedIndex | int? | getInt | selected-index |
| [x] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [x] | elevation | double? | getDouble | elevation |
| [x] | groupAlignment | double? | getDouble | group-alignment |
| [x] | labelType | NavigationRailLabelType? | getValue | label-type |
| [x] | unselectedLabelTextStyle | TextStyle? | getValue | unselected-label-text-style |
| [x] | selectedLabelTextStyle | TextStyle? | getValue | selected-label-text-style |
| [x] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [x] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [x] | minWidth | double? | getDouble | min-width |
| [x] | minExtendedWidth | double? | getDouble | min-extended-width |
| [x] | useIndicator | bool? | getBool | use-indicator |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [x] | leadingAtTop | bool | getBool | leading-at-top |
| [x] | trailingAtBottom | bool | getBool | trailing-at-bottom |
| [x] | scrollable | bool | getBool | scrollable |

#### [x] | TabBar | material/widgets/tab_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | tabs | List<Widget> | getWidgets | tabs |
| [x] | controller | TabController? | getValue | controller |
| [x] | isScrollable | bool | getBool | is-scrollable |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | automaticIndicatorColorAdjustment | bool | getBool | automatic-indicator-color-adjustment |
| [x] | indicatorWeight | double | getDouble | indicator-weight |
| [x] | indicatorPadding | EdgeInsetsGeometry | getValue | indicator-padding |
| [x] | indicator | Decoration? | getValue | indicator |
| [x] | indicatorSize | TabBarIndicatorSize? | getValue | indicator-size |
| [x] | dividerColor | Color? | getColor | divider-color |
| [x] | dividerHeight | double? | getDouble | divider-height |
| [x] | labelColor | Color? | getColor | label-color |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | unselectedLabelColor | Color? | getColor | unselected-label-color |
| [x] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | onTap | ValueChanged<int>? | getValue | on-tap |
| [x] | onHover | TabValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | TabValueChanged<bool>? | getValue | on-focus-change |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | splashFactory | InteractiveInkFeatureFactory? | getValue | splash-factory |
| [x] | splashBorderRadius | BorderRadius? | getValue | splash-border-radius |
| [x] | tabAlignment | TabAlignment? | getValue | tab-alignment |
| [x] | textScaler | TextScaler? | getValue | text-scaler |
| [x] | indicatorAnimation | TabIndicatorAnimation? | getValue | indicator-animation |

#### [x] | TextField | material/widgets/text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | groupId | Object | getValue | group-id |
| [x] | controller | TextEditingController? | getValue | controller |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | undoController | UndoHistoryController? | getValue | undo-controller |
| [x] | decoration | InputDecoration? | getValue | decoration |
| [x] | keyboardType | TextInputType? | getValue | keyboard-type |
| [x] | textInputAction | TextInputAction? | getValue | text-input-action |
| [x] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [x] | style | TextStyle? | getValue | style |
| [x] | strutStyle | StrutStyle? | getValue | strut-style |
| [x] | textAlign | TextAlign | getValue | text-align |
| [x] | textAlignVertical | TextAlignVertical? | getValue | text-align-vertical |
| [x] | textDirection | TextDirection? | getValue | text-direction |
| [x] | readOnly | bool | getBool | read-only |
| [x] | toolbarOptions | ToolbarOptions? | getValue | toolbar-options |
| [x] | showCursor | bool? | getBool | show-cursor |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | statesController | MaterialStatesController? | getValue | states-controller |
| [x] | obscuringCharacter | String | getString | obscuring-character |
| [x] | obscureText | bool | getBool | obscure-text |
| [x] | autocorrect | bool? | getBool | autocorrect |
| [x] | smartDashesType | SmartDashesType? | getValue | smart-dashes-type |
| [x] | smartQuotesType | SmartQuotesType? | getValue | smart-quotes-type |
| [x] | enableSuggestions | bool | getBool | enable-suggestions |
| [x] | maxLines | int? | getInt | max-lines |
| [x] | minLines | int? | getInt | min-lines |
| [x] | expands | bool | getBool | expands |
| [x] | maxLength | int? | getInt | max-length |
| [x] | maxLengthEnforcement | MaxLengthEnforcement? | getValue | max-length-enforcement |
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onEditingComplete | VoidCallback? | getVoidCallback | on-editing-complete |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | onAppPrivateCommand | AppPrivateCommandCallback? | getVoidCallback | on-app-private-command |
| [x] | inputFormatters | List<TextInputFormatter>? | getValue | input-formatters |
| [x] | enabled | bool? | getBool | enabled |
| [x] | ignorePointers | bool? | getBool | ignore-pointers |
| [x] | cursorWidth | double | getDouble | cursor-width |
| [x] | cursorHeight | double? | getDouble | cursor-height |
| [x] | cursorRadius | Radius? | getValue | cursor-radius |
| [x] | cursorOpacityAnimates | bool? | getBool | cursor-opacity-animates |
| [x] | cursorColor | Color? | getColor | cursor-color |
| [x] | cursorErrorColor | Color? | getColor | cursor-error-color |
| [x] | selectionHeightStyle | BoxHeightStyle? | getValue | selection-height-style |
| [x] | selectionWidthStyle | BoxWidthStyle? | getValue | selection-width-style |
| [x] | keyboardAppearance | Brightness? | getValue | keyboard-appearance |
| [x] | scrollPadding | EdgeInsets | getValue | scroll-padding |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | enableInteractiveSelection | bool? | getBool | enable-interactive-selection |
| [x] | selectAllOnFocus | bool? | getBool | select-all-on-focus |
| [x] | selectionControls | TextSelectionControls? | getValue | selection-controls |
| [x] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [x] | onTapAlwaysCalled | bool | getBool | on-tap-always-called |
| [x] | onTapOutside | TapRegionCallback? | getVoidCallback | on-tap-outside |
| [x] | onTapUpOutside | TapRegionUpCallback? | getVoidCallback | on-tap-up-outside |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | buildCounter | InputCounterWidgetBuilder? | getValue | build-counter |
| [x] | scrollController | ScrollController? | getValue | scroll-controller |
| [x] | scrollPhysics | ScrollPhysics? | getValue | scroll-physics |
| [x] | autofillHints | Iterable<String>? | getValue | autofill-hints |
| [x] | contentInsertionConfiguration | ContentInsertionConfiguration? | getValue | content-insertion-configuration |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | scribbleEnabled | bool | getBool | scribble-enabled |
| [x] | stylusHandwritingEnabled | bool | getBool | stylus-handwriting-enabled |
| [x] | enableIMEPersonalizedLearning | bool | getBool | enable-ime-personalized-learning |
| [x] | contextMenuBuilder | EditableTextContextMenuBuilder? | getValue | context-menu-builder |
| [x] | canRequestFocus | bool | getBool | can-request-focus |
| [x] | spellCheckConfiguration | SpellCheckConfiguration? | getValue | spell-check-configuration |
| [x] | magnifierConfiguration | TextMagnifierConfiguration? | getValue | magnifier-configuration |
| [x] | hintLocales | List<Locale>? | getValue | hint-locales |

#### [x] | Checkbox | material/widgets/checkbox.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | bool? | getBool | value |
| [x] | tristate | bool | getBool | tristate |
| [x] | onChanged | ValueChanged<bool?>? | getValue | on-changed |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [x] | checkColor | Color? | getColor | check-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | side | BorderSide? | getValue | side |
| [x] | isError | bool | getBool | is-error |
| [x] | semanticLabel | String? | getString | semantic-label |

#### [x] | Chip | material/widgets/chip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | avatar | Widget? | getWidget | avatar |
| [x] | label | Widget | properties.child | - |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | deleteIcon | Widget? | getWidget | delete-icon |
| [x] | onDeleted | VoidCallback? | getVoidCallback | on-deleted |
| [x] | deleteIconColor | Color? | getColor | delete-icon-color |
| [x] | deleteButtonTooltipMessage | String? | getString | delete-button-tooltip-message |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | color | WidgetStateProperty<Color?>? | getValue | color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | iconTheme | IconThemeData? | getValue | icon-theme |
| [x] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |
| [x] | deleteIconBoxConstraints | BoxConstraints? | getValue | delete-icon-box-constraints |
| [x] | chipAnimationStyle | ChipAnimationStyle? | getValue | chip-animation-style |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |

#### [x] | DatePickerDialog | material/widgets/date_picker_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | initialDate | DateTime? | getValue | initial-date |
| [x] | firstDate | DateTime | getValue | first-date |
| [x] | lastDate | DateTime | getValue | last-date |
| [x] | currentDate | DateTime? | getValue | current-date |
| [x] | initialEntryMode | DatePickerEntryMode | getValue | initial-entry-mode |
| [x] | selectableDayPredicate | SelectableDayPredicate? | getValue | selectable-day-predicate |
| [x] | cancelText | String? | getString | cancel-text |
| [x] | confirmText | String? | getString | confirm-text |
| [x] | helpText | String? | getString | help-text |
| [x] | errorFormatText | String? | getString | error-format-text |
| [x] | errorInvalidText | String? | getString | error-invalid-text |
| [x] | fieldHintText | String? | getString | field-hint-text |
| [x] | fieldLabelText | String? | getString | field-label-text |
| [x] | keyboardType | TextInputType? | getValue | keyboard-type |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | onDatePickerModeChange | ValueChanged<DatePickerEntryMode>? | getValue | on-date-picker-mode-change |
| [x] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [x] | switchToCalendarEntryModeIcon | Icon? | getValue | switch-to-calendar-entry-mode-icon |
| [x] | insetPadding | EdgeInsets | getValue | inset-padding |
| [x] | calendarDelegate | CalendarDelegate<DateTime> | getValue | calendar-delegate |

#### [x] | MenuAnchor | material/widgets/menu_anchor.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | controller | MenuController? | getValue | controller |
| [x] | childFocusNode | FocusNode? | getValue | child-focus-node |
| [x] | style | MenuStyle? | getValue | style |
| [x] | alignmentOffset | Offset | getValue | alignment-offset |
| [x] | reservedPadding | EdgeInsetsGeometry? | getValue | reserved-padding |
| [x] | layerLink | LayerLink? | getValue | layer-link |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | anchorTapClosesMenu | bool | getBool | anchor-tap-closes-menu |
| [x] | consumeOutsideTap | bool | getBool | consume-outside-tap |
| [x] | onOpen | VoidCallback? | getVoidCallback | on-open |
| [x] | onClose | VoidCallback? | getVoidCallback | on-close |
| [x] | crossAxisUnconstrained | bool | getBool | cross-axis-unconstrained |
| [x] | useRootOverlay | bool | getBool | use-root-overlay |
| [x] | menuChildren | List<Widget> | getWidgets | menu-children |
| [x] | builder | MenuAnchorChildBuilder? | getValue | builder |
| [x] | child | Widget? | properties.child | - |

#### [x] | Radio | material/widgets/radio.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | T | getValue | value |
| [x] | groupValue | T? | getValue | group-value |
| [x] | onChanged | ValueChanged<T?>? | getValue | on-changed |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | toggleable | bool | getBool | toggleable |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | enabled | bool? | getBool | enabled |
| [x] | groupRegistry | RadioGroupRegistry<T>? | getValue | group-registry |
| [x] | backgroundColor | WidgetStateProperty<Color?>? | getValue | background-color |
| [x] | side | BorderSide? | getValue | side |
| [x] | innerRadius | WidgetStateProperty<double?>? | getValue | inner-radius |

#### [x] | Slider | material/widgets/slider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | double | getDouble | value |
| [x] | secondaryTrackValue | double? | getDouble | secondary-track-value |
| [x] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [x] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [x] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [x] | min | double | getDouble | min |
| [x] | max | double | getDouble | max |
| [x] | divisions | int? | getInt | divisions |
| [x] | label | String? | getString | label |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | inactiveColor | Color? | getColor | inactive-color |
| [x] | secondaryActiveColor | Color? | getColor | secondary-active-color |
| [x] | thumbColor | Color? | getColor | thumb-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | semanticFormatterCallback | SemanticFormatterCallback? | getValue | semantic-formatter-callback |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | allowedInteraction | SliderInteraction? | getValue | allowed-interaction |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | year2023 | bool? | getBool | year2023 |

#### [x] | Switch | material/widgets/switch.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | bool | getBool | value |
| [x] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | activeThumbColor | Color? | getColor | active-thumb-color |
| [x] | activeTrackColor | Color? | getColor | active-track-color |
| [x] | inactiveThumbColor | Color? | getColor | inactive-thumb-color |
| [x] | inactiveTrackColor | Color? | getColor | inactive-track-color |
| [x] | activeThumbImage | ImageProvider<Object>? | getValue | active-thumb-image |
| [x] | onActiveThumbImageError | ImageErrorListener? | getValue | on-active-thumb-image-error |
| [x] | inactiveThumbImage | ImageProvider<Object>? | getValue | inactive-thumb-image |
| [x] | onInactiveThumbImageError | ImageErrorListener? | getValue | on-inactive-thumb-image-error |
| [x] | thumbColor | WidgetStateProperty<Color?>? | getValue | thumb-color |
| [x] | trackColor | WidgetStateProperty<Color?>? | getValue | track-color |
| [x] | trackOutlineColor | WidgetStateProperty<Color?>? | getValue | track-outline-color |
| [x] | trackOutlineWidth | WidgetStateProperty<double?>? | getValue | track-outline-width |
| [x] | thumbIcon | WidgetStateProperty<Icon?>? | getValue | thumb-icon |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |

#### [x] | TimePickerDialog | material/widgets/time_picker_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | initialTime | TimeOfDay | getValue | initial-time |
| [x] | cancelText | String? | getString | cancel-text |
| [x] | confirmText | String? | getString | confirm-text |
| [x] | helpText | String? | getString | help-text |
| [x] | errorInvalidText | String? | getString | error-invalid-text |
| [x] | hourLabelText | String? | getString | hour-label-text |
| [x] | minuteLabelText | String? | getString | minute-label-text |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | initialEntryMode | TimePickerEntryMode | getValue | initial-entry-mode |
| [x] | orientation | Orientation? | getValue | orientation |
| [x] | onEntryModeChanged | EntryModeChangeCallback? | getValue | on-entry-mode-changed |
| [x] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [x] | switchToTimerEntryModeIcon | Icon? | getValue | switch-to-timer-entry-mode-icon |
| [x] | emptyInitialInput | bool | getBool | empty-initial-input |

#### [x] | Drawer | material/widgets/drawer.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | width | double? | getDouble | width |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | semanticLabel | String? | getString | semantic-label |
| [x] | child | Widget? | properties.child | - |

#### [x] | BottomNavigationBar | material/widgets/bottom_navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | items | List<BottomNavigationBarItem> | getValue | items |
| [x] | onTap | ValueChanged<int>? | getValue | on-tap |
| [x] | currentIndex | int | getInt | current-index |
| [x] | elevation | double | getDouble | elevation |
| [x] | type | BottomNavigationBarType? | getValue | type |
| [x] | fixedColor | Color? | getColor | fixed-color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | selectedItemColor | Color? | getColor | selected-item-color |
| [x] | unselectedItemColor | Color? | getColor | unselected-item-color |
| [x] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [x] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [x] | selectedLabelStyle | TextStyle? | getValue | selected-label-style |
| [x] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [x] | selectedFontSize | double | getDouble | selected-font-size |
| [x] | unselectedFontSize | double | getDouble | unselected-font-size |
| [x] | showSelectedLabels | bool? | getBool | show-selected-labels |
| [x] | showUnselectedLabels | bool? | getBool | show-unselected-labels |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | landscapeLayout | BottomNavigationBarLandscapeLayout? | getValue | landscape-layout |

#### [x] | DropdownButton | material/widgets/dropdown_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | items | List<DropdownMenuItem>? | getValue | items |
| [x] | selectedItemBuilder | DropdownButtonBuilder? | getValue | selected-item-builder |
| [x] | value | T? | getValue | value |
| [x] | hint | Widget? | getWidget | hint |
| [x] | disabledHint | Widget? | getWidget | disabled-hint |
| [x] | onChanged | ValueChanged? | getValue | on-changed |
| [x] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [x] | elevation | int | getInt | elevation |
| [x] | style | TextStyle? | getValue | style |
| [x] | underline | Widget? | getWidget | underline |
| [x] | icon | Widget? | getWidget | icon |
| [x] | iconDisabledColor | Color? | getColor | icon-disabled-color |
| [x] | iconEnabledColor | Color? | getColor | icon-enabled-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | isDense | bool | getBool | is-dense |
| [x] | isExpanded | bool | getBool | is-expanded |
| [x] | itemHeight | double? | getDouble | item-height |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | dropdownColor | Color? | getColor | color |
| [x] | menuMaxHeight | double? | getDouble | menu-max-height |
| [x] | enableFeedback | bool | getBool | enable-feedback |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | borderRadius | BorderRadius? | getValue | border-radius |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |

#### [x] | CircularProgressIndicator | material/widgets/circular_progress_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | double? | getDouble | value |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | color | Color? | getColor | color |
| [x] | strokeWidth | double? | getDouble | stroke-width |
| [x] | strokeAlign | double? | getDouble | stroke-align |
| [x] | strokeCap | StrokeCap? | getValue | stroke-cap |
| [x] | semanticsLabel | String? | getString | semantics-label |
| [x] | semanticsValue | String? | getString | semantics-value |

#### [x] | RefreshIndicator | material/widgets/refresh_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | child | Widget | properties.child | - |
| [x] | displacement | double | getDouble | displacement |
| [x] | edgeOffset | double | getDouble | edge-offset |
| [x] | onRefresh | RefreshCallback | getValue | on-refresh |
| [x] | color | Color? | getColor | color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | notificationPredicate | ScrollNotificationPredicate | getValue | notification-predicate |
| [x] | semanticsLabel | String? | getString | semantics-label |
| [x] | semanticsValue | String? | getString | semantics-value |
| [x] | strokeWidth | double | getDouble | stroke-width |
| [x] | triggerMode | RefreshIndicatorTriggerMode | getValue | trigger-mode |

#### [x] | SearchBar | material/widgets/search_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | controller | TextEditingController? | getValue | controller |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | hintText | String? | getString | hint-text |
| [x] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | elevation | WidgetStateProperty? | getValue | elevation |
| [x] | overlayColor | WidgetStateProperty? | getValue | overlay-color |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | textStyle | TextStyle? | getValue | text-style |
| [x] | hintStyle | TextStyle? | getValue | hint-style |
| [x] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [x] | keyboardType | TextInputType | getValue | keyboard-type |

#### [x] | ActionChip | material/widgets/action_chip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | avatar | Widget? | getValue | avatar |
| [x] | label | Widget | getValue | label |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | pressElevation | double? | getValue | press-elevation |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |

#### [x] | TabBarView | material/widgets/tab_bar_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | children | List<Widget> | getWidgets | children |
| [x] | controller | TabController? | getValue | controller |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | viewportFraction | double | getDouble | viewport-fraction |

#### [x] | Tooltip | material/widgets/tooltip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | message | String? | getString | tooltip-message |
| [x] | richMessage | InlineSpan? | getValue | tooltip-rich-message |
| [x] | padding | EdgeInsetsGeometry? | getValue | tooltip-padding |
| [x] | margin | EdgeInsetsGeometry? | getValue | tooltip-margin |
| [x] | verticalOffset | double? | getDouble | tooltip-vertical-offset |
| [x] | preferBelow | bool? | getBool | tooltip-prefer-below |
| [x] | excludeFromSemantics | bool? | getBool | tooltip-exclude-from-semantics |
| [x] | decoration | Decoration? | getValue | tooltip-decoration |
| [x] | textStyle | TextStyle? | getValue | tooltip-text-style |
| [x] | textAlign | TextAlign? | getValue | tooltip-text-align |
| [x] | waitDuration | Duration? | getValue | tooltip-wait-duration |
| [x] | showDuration | Duration? | getValue | tooltip-show-duration |
| [x] | triggerMode | TooltipTriggerMode? | getValue | tooltip-trigger-mode |
| [x] | enableFeedback | bool? | getBool | tooltip-enable-feedback |
| [x] | onTriggered | VoidCallback? | getVoidCallback | tooltip-on-triggered |
| [x] | child | Widget? | properties.child | - |
