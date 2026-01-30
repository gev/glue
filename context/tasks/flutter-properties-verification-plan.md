

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

#### [ ] | AppBar | core/widgets/app_bar.dart
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

#### [ ] | Column | core/widgets/column.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | mainAxisAlignment | MainAxisAlignment | getValue | main-axis-alignment |
| [ ] | mainAxisSize | MainAxisSize | getValue | main-axis-size |
| [ ] | crossAxisAlignment | CrossAxisAlignment | getValue | cross-axis-alignment |
| [ ] | textDirection | TextDirection? | getValue | text-direction |
| [ ] | verticalDirection | VerticalDirection | getValue | vertical-direction |
| [ ] | textBaseline | TextBaseline? | getValue | text-baseline |
| [ ] | children | List<Widget> | properties.children | - |

#### [ ] | Container | core/widgets/container.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | alignment | AlignmentGeometry? | getValue | alignment |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | color | Color? | getColor | color |
| [ ] | decoration | Decoration? | getValue | decoration |
| [ ] | foregroundDecoration | Decoration? | getValue | foreground-decoration |
| [ ] | width | double? | properties.width | - |
| [ ] | height | double? | properties.height | - |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | margin | EdgeInsetsGeometry? | getValue | margin |
| [ ] | transform | Matrix4? | getValue | transform |
| [ ] | transformAlignment | AlignmentGeometry? | getValue | transform-alignment |
| [ ] | child | Widget? | properties.child | - |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |

#### [ ] | ElevatedButton | core/widgets/elevated_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | style | ButtonStyle? | getValue | style |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | child | Widget | properties.child | - |

#### [ ] | FlutterLogo | core/widgets/flutter_logo.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | size | double | getDouble | size |
| [ ] | style | FlutterLogoStyle | getValue | style |
| [ ] | textColor | Color? | getColor | text-color |
| [ ] | colors | Color? | getColor | colors |
| [ ] | duration | Duration | getValue | duration |
| [ ] | curve | Curve | getValue | curve |

#### [ ] | Icon | core/widgets/icon.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | icon | IconData | getValue | icon |
| [ ] | size | double? | getDouble | size |
| [ ] | color | Color? | getColor | color |
| [ ] | semanticLabel | String? | getString | semantic-label |
| [ ] | textDirection | TextDirection? | getValue | text-direction |

#### [ ] | Image | core/widgets/image.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | image | ImageProvider<Object> | getValue | image |
| [ ] | width | double? | properties.width | - |
| [ ] | height | double? | properties.height | - |
| [ ] | color | Color? | getColor | color |
| [ ] | colorBlendMode | BlendMode? | getValue | color-blend-mode |
| [ ] | fit | BoxFit? | getValue | fit |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | repeat | ImageRepeat | getValue | repeat |
| [ ] | centerSlice | Rect? | getValue | center-slice |
| [ ] | matchTextDirection | bool | getBool | match-text-direction |
| [ ] | gaplessPlayback | bool | getBool | gapless-playback |
| [ ] | semanticLabel | String? | getString | semantic-label |
| [ ] | excludeFromSemantics | bool | getBool | exclude-from-semantics |
| [ ] | filterQuality | FilterQuality | getValue | filter-quality |
| [ ] | cacheWidth | int? | getInt | cache-width |
| [ ] | cacheHeight | int? | getInt | cache-height |

#### [ ] | Placeholder | core/widgets/placeholder.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | fallbackWidth | double | getDouble | fallback-width |
| [ ] | fallbackHeight | double | getDouble | fallback-height |
| [ ] | color | Color? | getColor | color |
| [ ] | strokeAlign | StrokeAlign | getValue | stroke-align |
| [ ] | strokeWidth | double | getDouble | stroke-width |

#### [ ] | Row | core/widgets/row.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | mainAxisAlignment | MainAxisAlignment | getValue | main-axis-alignment |
| [ ] | mainAxisSize | MainAxisSize | getValue | main-axis-size |
| [ ] | crossAxisAlignment | CrossAxisAlignment | getValue | cross-axis-alignment |
| [ ] | textDirection | TextDirection? | getValue | text-direction |
| [ ] | verticalDirection | VerticalDirection | getValue | vertical-direction |
| [ ] | textBaseline | TextBaseline? | getValue | text-baseline |
| [ ] | children | List<Widget> | properties.children | - |

#### [ ] | Scaffold | core/widgets/scaffold.dart
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

#### [ ] | Text | core/widgets/text.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | data | String | - | data |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | strutStyle | StrutStyle? | getValue | strut-style |
| [ ] | textAlign | TextAlign? | getValue | text-align |
| [ ] | textDirection | TextDirection? | getValue | text-direction |
| [ ] | locale | Locale? | getValue | locale |
| [ ] | softWrap | bool? | getBool | soft-wrap |
| [ ] | overflow | TextOverflow? | getValue | overflow |
| [ ] | textScaleFactor | double? | getDouble | text-scale-factor |
| [ ] | maxLines | int? | getInt | max-lines |
| [ ] | semanticsLabel | String? | getString | semantics-label |
| [ ] | textWidthBasis | TextWidthBasis? | getValue | text-width-basis |
| [ ] | textHeightBehavior | TextHeightBehavior? | getValue | text-height-behavior |

#### [ ] | Center | core/widgets/center.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget? | properties.child | - |

#### [ ] | Padding | core/widgets/padding.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |
| [ ] | child | Widget? | properties.child | - |

#### [x] | ListView | core/widgets/list_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | scrollDirection | Axis | getValue | scroll-direction |
| [ ] | reverse | bool | getValue | reverse |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | primary | bool? | getValue | primary |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | shrinkWrap | bool | getValue | shrink-wrap |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | itemExtent | double? | getDouble | item-extent |
| [ ] | prototypeItem | Widget? | getWidget | prototype-item |
| [ ] | addAutomaticKeepAlives | bool | getValue | add-automatic-keep-alives |
| [ ] | addRepaintBoundaries | bool | getValue | add-repaint-boundaries |
| [ ] | addSemanticIndexes | bool | getValue | add-semantic-indexes |
| [ ] | cacheExtent | double? | getDouble | cache-extent |
| [ ] | children | List<Widget> | properties.children | - |
| [ ] | semanticChildCount | int? | getInt | semantic-child-count |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |

#### [ ] | GridView | core/widgets/grid_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | scrollDirection | Axis | getValue | scroll-direction |
| [ ] | reverse | bool | getValue | reverse |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | primary | bool? | getValue | primary |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | shrinkWrap | bool | getValue | shrink-wrap |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | gridDelegate | SliverGridDelegate | getValue | grid-delegate |
| [ ] | addAutomaticKeepAlives | bool | getValue | add-automatic-keep-alives |
| [ ] | addRepaintBoundaries | bool | getValue | add-repaint-boundaries |
| [ ] | addSemanticIndexes | bool | getValue | add-semantic-indexes |
| [ ] | cacheExtent | double? | getDouble | cache-extent |
| [ ] | children | List<Widget> | properties.children | - |
| [ ] | semanticChildCount | int? | getInt | semantic-child-count |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |

#### [ ] | SingleChildScrollView | core/widgets/single_child_scroll_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | scrollDirection | Axis | getValue | scroll-direction |
| [ ] | reverse | bool | getValue | reverse |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | primary | bool? | getValue | primary |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [ ] | child | Widget? | properties.child | - |

#### [ ] | CustomScrollView | core/widgets/custom_scroll_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | scrollDirection | Axis | getValue | scroll-direction |
| [ ] | reverse | bool | getBool | reverse |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | primary | bool? | getBool | primary |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | shrinkWrap | bool | getBool | shrink-wrap |
| [ ] | center | Key? | getKey | center |
| [ ] | anchor | double | getDouble | anchor |
| [ ] | cacheExtent | double? | getDouble | cache-extent |
| [ ] | slivers | List<Widget> | getWidgets | slivers |
| [ ] | semanticChildCount | int? | getInt | semantic-child-count |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |

#### [ ] | SliverGrid | core/widgets/sliver_grid.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | delegate | SliverChildDelegate | getValue | sliver-grid-delegate |
| [ ] | gridDelegate | SliverGridDelegate | getValue | sliver-grid-grid-delegate |

#### [ ] | SliverList | core/widgets/sliver_list.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | delegate | SliverChildDelegate | getValue | sliver-list-delegate |

### Cupertino Module: flutter/glue_flutter/lib/src/lib/ui/cupertino/

#### [ ] | CupertinoActionSheet | cupertino/widgets/cupertino_action_sheet.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | title | Widget? | getWidget | title |
| [ ] | message | Widget? | getWidget | message |
| [ ] | actions | List<Widget>? | getWidgets | actions |
| [ ] | messageScrollController | ScrollController? | getValue | message-scroll-controller |
| [ ] | actionScrollController | ScrollController? | getValue | action-scroll-controller |
| [ ] | cancelButton | Widget? | getWidget | cancel-button |

#### [ ] | CupertinoActivityIndicator | cupertino/widgets/cupertino_activity_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | color | Color? | getColor | color |
| [ ] | animating | bool | getBool | animating |
| [ ] | radius | double | getDouble | radius |

#### [ ] | CupertinoAlertDialog | cupertino/widgets/cupertino_alert_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | title | Widget? | getWidget | title |
| [ ] | content | Widget? | getWidget | content |
| [ ] | actions | List<Widget> | getWidgets | actions |
| [ ] | scrollController | ScrollController? | getValue | scroll-controller |
| [ ] | actionScrollController | ScrollController? | getValue | action-scroll-controller |
| [ ] | insetAnimationDuration | Duration | getValue | inset-animation-duration |
| [ ] | insetAnimationCurve | Curve | getValue | inset-animation-curve |

#### [ ] | CupertinoApp | cupertino/widgets/cupertino_app.dart
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

#### [ ] | CupertinoButton | cupertino/widgets/cupertino_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget | properties.child | - |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | color | Color? | getColor | color |
| [ ] | disabledColor | Color | getColor | disabled-color |
| [ ] | minSize | double | getDouble | min-size |
| [ ] | pressedOpacity | double | getDouble | pressed-opacity |
| [ ] | borderRadius | BorderRadius | getValue | border-radius |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |

#### [ ] | CupertinoCheckbox | cupertino/widgets/cupertino_checkbox.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | bool? | getBool | value |
| [ ] | tristate | bool | getBool | tristate |
| [ ] | onChanged | ValueChanged<bool?>? | getValue | on-changed |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | inactiveColor | Color? | getColor | inactive-color |
| [ ] | checkColor | Color? | getColor | check-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | autofocus | bool | getBool | autofocus |

#### [ ] | CupertinoContextMenu | cupertino/widgets/cupertino_context_menu.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | actions | List<Widget> | getWidgets | actions |
| [ ] | child | Widget | properties.child | - |
| [ ] | previewBuilder | WidgetBuilder? | getValue | preview-builder |

#### [ ] | CupertinoDatePicker | cupertino/widgets/cupertino_date_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | mode | CupertinoDatePickerMode | getValue | mode |
| [ ] | onDateTimeChanged | ValueChanged<DateTime> | getValue | on-date-time-changed |
| [ ] | initialDateTime | DateTime? | getValue | initial-date-time |
| [ ] | minimumDate | DateTime? | getValue | minimum-date |
| [ ] | maximumDate | DateTime? | getValue | maximum-date |
| [ ] | minimumYear | int | getInt | minimum-year |
| [ ] | maximumYear | int? | getInt | maximum-year |
| [ ] | minuteInterval | int | getInt | minute-interval |
| [ ] | use24hFormat | bool | getBool | use24h-format |
| [ ] | dateOrder | DatePickerDateOrder? | getValue | date-order |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | showDayOfWeek | bool | getBool | show-day-of-week |
| [ ] | itemExtent | double | getDouble | item-extent |

#### [ ] | CupertinoNavigationBar | cupertino/widgets/cupertino_navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | automaticallyImplyLeading | bool | getBool | automatically-imply-leading |
| [ ] | automaticallyImplyMiddle | bool | getBool | automatically-imply-middle |
| [ ] | previousPageTitle | String? | getString | previous-page-title |
| [ ] | middle | Widget? | getWidget | middle |
| [ ] | trailing | Widget? | getWidget | trailing |
| [ ] | border | Border? | getValue | border |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | brightness | Brightness? | getValue | brightness |
| [ ] | padding | EdgeInsetsDirectional? | getValue | padding |
| [ ] | transitionBetweenRoutes | bool | getBool | transition-between-routes |
| [ ] | heroTag | Object | getValue | hero-tag |

#### [ ] | CupertinoPageScaffold | cupertino/widgets/cupertino_page_scaffold.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | navigationBar | ObstructingPreferredSizeWidget? | getValue | navigation-bar |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | resizeToAvoidBottomInset | bool | getBool | resize-to-avoid-bottom-inset |
| [ ] | child | Widget | properties.child | - |

#### [ ] | CupertinoPicker | cupertino/widgets/cupertino_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | diameterRatio | double | getDouble | diameter-ratio |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | offAxisFraction | double | getDouble | off-axis-fraction |
| [ ] | useMagnifier | bool | getBool | use-magnifier |
| [ ] | magnification | double | getDouble | magnification |
| [ ] | scrollController | FixedExtentScrollController? | getValue | scroll-controller |
| [ ] | squeeze | double | getDouble | squeeze |
| [ ] | itemExtent | double | getDouble | item-extent |
| [ ] | onSelectedItemChanged | ValueChanged<int>? | getValue | on-selected-item-changed |
| [ ] | children | List<Widget> | properties.children | - |
| [ ] | selectionOverlay | Widget | getValue | selection-overlay |

#### [ ] | CupertinoScrollbar | cupertino/widgets/cupertino_scrollbar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget | properties.child | - |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | thumbVisibility | bool? | getBool | thumb-visibility |
| [ ] | thickness | double | getDouble | thickness |
| [ ] | thicknessWhileDragging | double | getDouble | thickness-while-dragging |
| [ ] | radius | Radius | getValue | radius |
| [ ] | radiusWhileDragging | Radius | getValue | radius-while-dragging |
| [ ] | notificationPredicate | ScrollNotificationPredicate? | getValue | notification-predicate |

#### [ ] | CupertinoSearchTextField | cupertino/widgets/cupertino_search_text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | controller | TextEditingController? | getValue | controller |
| [ ] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [ ] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | placeholder | String? | getString | placeholder |
| [ ] | placeholderStyle | TextStyle? | getValue | placeholder-style |
| [ ] | decoration | BoxDecoration? | getValue | decoration |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | borderRadius | BorderRadius? | getValue | border-radius |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |
| [ ] | itemColor | Color? | getColor | item-color |
| [ ] | itemSize | double? | getDouble | item-size |
| [ ] | prefixIcon | Widget? | getWidget | prefix-icon |
| [ ] | prefixMode | OverlayVisibilityMode | getValue | prefix-mode |
| [ ] | suffixIcon | Widget? | getWidget | suffix-icon |
| [ ] | suffixMode | OverlayVisibilityMode | getValue | suffix-mode |
| [ ] | onSuffixTap | VoidCallback? | getVoidCallback | on-suffix-tap |
| [ ] | enabled | bool? | getBool | enabled |
| [ ] | autocorrect | bool | getBool | autocorrect |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |

#### [ ] | CupertinoSegmentedControl | cupertino/widgets/cupertino_segmented_control.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | children | Map<T, Widget> | getValue | children |
| [ ] | onValueChanged | ValueChanged<T>? | getValue | on-value-changed |
| [ ] | groupValue | T? | getValue | group-value |
| [ ] | unselectedColor | Color | getColor | unselected-color |
| [ ] | selectedColor | Color | getColor | selected-color |
| [ ] | borderColor | Color | getColor | border-color |
| [ ] | pressedColor | Color? | getColor | pressed-color |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |

#### [ ] | CupertinoSlider | cupertino/widgets/cupertino_slider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | double | getDouble | value |
| [ ] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [ ] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [ ] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [ ] | min | double | getDouble | min |
| [ ] | max | double | getDouble | max |
| [ ] | divisions | int? | getInt | divisions |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | thumbColor | Color | getColor | thumb-color |

#### [ ] | CupertinoSlidingSegmentedControl | cupertino/widgets/cupertino_sliding_segmented_control.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | children | Map<T, Widget> | getValue | children |
| [ ] | onValueChanged | ValueChanged<T?>? | getValue | on-value-changed |
| [ ] | groupValue | T? | getValue | group-value |
| [ ] | thumbColor | Color | getColor | thumb-color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |

#### [ ] | CupertinoSwitch | cupertino/widgets/cupertino_switch.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | bool | getBool | value |
| [ ] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | trackColor | Color? | getColor | track-color |
| [ ] | thumbColor | Color? | getColor | thumb-color |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |

#### [ ] | CupertinoTabBar | cupertino/widgets/cupertino_tab_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | items | List<BottomNavigationBarItem> | getValue | items |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | currentIndex | int | getInt | current-index |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | inactiveColor | Color | getColor | inactive-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | border | Border? | getValue | border |

#### [ ] | CupertinoTabScaffold | cupertino/widgets/cupertino_tab_scaffold.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | tabBar | CupertinoTabBar | getValue | tab-bar |
| [ ] | tabBuilder | IndexedWidgetBuilder | getValue | tab-builder |
| [ ] | controller | CupertinoTabController? | getValue | controller |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | resizeToAvoidBottomInset | bool | getBool | resize-to-avoid-bottom-inset |
| [ ] | restorationId | String? | getString | restoration-id |

#### [ ] | CupertinoTextField | cupertino/widgets/cupertino_text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | controller | TextEditingController? | getValue | controller |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | decoration | BoxDecoration | getValue | decoration |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |
| [ ] | placeholder | String? | getString | placeholder |
| [ ] | placeholderStyle | TextStyle | getValue | placeholder-style |
| [ ] | prefix | Widget? | getWidget | prefix |
| [ ] | prefixMode | OverlayVisibilityMode | getValue | prefix-mode |
| [ ] | suffix | Widget? | getWidget | suffix |
| [ ] | suffixMode | OverlayVisibilityMode | getValue | suffix-mode |
| [ ] | clearButtonMode | OverlayVisibilityMode | getValue | clear-button-mode |
| [ ] | keyboardType | TextInputType? | getValue | keyboard-type |
| [ ] | textInputAction | TextInputAction? | getValue | text-input-action |
| [ ] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | strutStyle | StrutStyle? | getValue | strut-style |
| [ ] | textAlign | TextAlign | getValue | text-align |
| [ ] | textAlignVertical | TextAlignVertical? | getValue | text-align-vertical |
| [ ] | readOnly | bool | getBool | read-only |
| [ ] | showCursor | bool? | getBool | show-cursor |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | obscuringCharacter | String | getString | obscuring-character |
| [ ] | obscureText | bool | getBool | obscure-text |
| [ ] | autocorrect | bool | getBool | autocorrect |
| [ ] | smartDashesType | SmartDashesType? | getValue | smart-dashes-type |
| [ ] | smartQuotesType | SmartQuotesType? | getValue | smart-quotes-type |
| [ ] | enableSuggestions | bool | getBool | enable-suggestions |
| [ ] | maxLines | int? | getInt | max-lines |
| [ ] | minLines | int? | getInt | min-lines |
| [ ] | expands | bool | getBool | expands |
| [ ] | maxLength | int? | getInt | max-length |
| [ ] | maxLengthEnforcement | MaxLengthEnforcement? | getValue | max-length-enforcement |
| [ ] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [ ] | onEditingComplete | VoidCallback? | getVoidCallback | on-editing-complete |
| [ ] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [ ] | inputFormatters | List<TextInputFormatter>? | getValue | input-formatters |
| [ ] | enabled | bool? | getBool | enabled |
| [ ] | cursorWidth | double | getDouble | cursor-width |
| [ ] | cursorHeight | double? | getDouble | cursor-height |
| [ ] | cursorRadius | Radius | getValue | cursor-radius |
| [ ] | cursorColor | Color? | getColor | cursor-color |
| [ ] | keyboardAppearance | Brightness? | getValue | keyboard-appearance |
| [ ] | scrollPadding | EdgeInsets | getValue | scroll-padding |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | enableInteractiveSelection | bool | getBool | enable-interactive-selection |
| [ ] | selectionControls | TextSelectionControls? | getValue | selection-controls |
| [ ] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [ ] | scrollController | ScrollController? | getValue | scroll-controller |
| [ ] | scrollPhysics | ScrollPhysics? | getValue | scroll-physics |
| [ ] | autofillHints | Iterable<String>? | getValue | autofill-hints |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | scribbleEnabled | bool | getBool | scribble-enabled |
| [ ] | enableIMEPersonalizedLearning | bool | getBool | enable-ime-personalized-learning |

#### [ ] | CupertinoTimerPicker | cupertino/widgets/cupertino_timer_picker.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | mode | CupertinoTimerPickerMode | getValue | mode |
| [ ] | initialTimerDuration | Duration | getValue | initial-timer-duration |
| [ ] | minuteInterval | int | getInt | minute-interval |
| [ ] | secondInterval | int | getInt | second-interval |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | itemExtent | double | getDouble | item-extent |
| [ ] | onTimerDurationChanged | ValueChanged<Duration> | getValue | on-timer-duration-changed |

### Material Module: flutter/glue_flutter/lib/src/lib/ui/material/

#### [ ] | ElevatedButton | material/widgets/elevated_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | style | ButtonStyle? | getValue | style |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | child | Widget | properties.child | - |

#### [ ] | FilledButton | material/widgets/filled_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | style | ButtonStyle? | getValue | style |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | child | Widget | properties.child | - |

#### [ ] | FloatingActionButton | material/widgets/floating_action_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget? | properties.child | - |
| [ ] | tooltip | String? | getString | tooltip |
| [ ] | foregroundColor | Color? | getColor | foreground-color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | heroTag | Object? | getValue | hero-tag |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | focusElevation | double? | getDouble | focus-elevation |
| [ ] | hoverElevation | double? | getDouble | hover-elevation |
| [ ] | highlightElevation | double? | getDouble | highlight-elevation |
| [ ] | disabledElevation | double? | getDouble | disabled-elevation |
| [ ] | mini | bool? | getBool | mini |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | isExtended | bool | getBool | is-extended |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | alignment | Alignment? | getValue | alignment |
| [ ] | offset | Offset? | getValue | offset |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | restorationId | String? | getString | restoration-id |

#### [ ] | IconButton | material/widgets/icon_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | icon | Widget? | properties.child | - |
| [ ] | color | Color? | getColor | color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | highlightColor | Color? | getColor | highlight-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | disabledColor | Color? | getColor | disabled-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | tooltip | String? | getString | tooltip |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | mini | bool? | getBool | mini |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | restorationId | String? | getString | restoration-id |

#### [ ] | OutlinedButton | material/widgets/outlined_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | style | ButtonStyle? | getValue | style |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | child | Widget | properties.child | - |

#### [ ] | SegmentedButton | material/widgets/segmented_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | selected | Set<T> | getValue | selected |
| [ ] | segments | List<Widget> | getWidgets | segments |
| [ ] | onSelectionChanged | ValueChanged<Set<T>> | getValue | on-selection-changed |
| [ ] | multiSelectionEnabledFor | Set<T>? | getValue | multi-selection-enabled-for |
| [ ] | showSelectedIcon | bool? | getBool | show-selected-icon |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | unselectedColor | Color? | getColor | unselected-color |
| [ ] | selectedColor | Color? | getColor | selected-color |
| [ ] | disabledColor | Color? | getColor | disabled-color |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |

#### [ ] | TextButton | material/widgets/text_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | onHover | ValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | style | ButtonStyle? | getValue | style |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | child | Widget | properties.child | - |

#### [ ] | Badge | material/widgets/badge.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget | properties.child | - |
| [ ] | label | Widget? | getWidget | label |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | textColor | Color? | getColor | text-color |
| [ ] | textStyle | TextStyle? | getValue | text-style |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | alignment | AlignmentGeometry? | getValue | alignment |
| [ ] | isLabelVisible | bool? | getBool | is-label-visible |
| [ ] | largeSize | bool? | getBool | large-size |
| [ ] | offset | Offset? | getValue | offset |
| [ ] | showBadge | bool? | getBool | show-badge |

#### [ ] | LinearProgressIndicator | material/widgets/linear_progress_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | double? | getDouble | value |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | color | Color? | getColor | color |
| [ ] | valueColor | Animation<Color>? | getValue | value-color |
| [ ] | minHeight | double? | getDouble | min-height |
| [ ] | semanticsLabel | String? | getString | semantics-label |
| [ ] | semanticsValue | String? | getString | semantics-value |

#### [ ] | SnackBar | material/widgets/snack_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | content | Widget | properties.child | - |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | margin | EdgeInsetsGeometry? | getValue | margin |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | width | double? | properties.width | - |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | behavior | SnackBarBehavior? | getValue | behavior |
| [ ] | action | SnackBarAction? | getValue | action |
| [ ] | duration | Duration? | getValue | duration |
| [ ] | animation | Animation<double>? | getValue | animation |
| [ ] | onVisible | VoidCallback? | getVoidCallback | on-visible |
| [ ] | dismissDirection | DismissDirection? | getValue | dismiss-direction |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |

#### [ ] | AlertDialog | material/widgets/alert_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | icon | Widget? | getWidget | icon |
| [ ] | iconPadding | EdgeInsetsGeometry? | getValue | icon-padding |
| [ ] | iconColor | Color? | getColor | icon-color |
| [ ] | title | Widget? | getWidget | title |
| [ ] | titlePadding | EdgeInsetsGeometry? | getValue | title-padding |
| [ ] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [ ] | content | Widget? | getWidget | content |
| [ ] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [ ] | contentTextStyle | TextStyle? | getValue | content-text-style |
| [ ] | actions | List<Widget>? | getWidgets | actions |
| [ ] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [ ] | actionsAlignment | MainAxisAlignment? | getValue | actions-alignment |
| [ ] | actionsOverflowAlignment | OverflowBarAlignment? | getValue | actions-overflow-alignment |
| [ ] | actionsOverflowDirection | VerticalDirection? | getValue | actions-overflow-direction |
| [ ] | actionsOverflowButtonSpacing | double? | getDouble | actions-overflow-button-spacing |
| [ ] | buttonPadding | EdgeInsetsGeometry? | getValue | button-padding |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | semanticLabel | String? | getString | semantic-label |
| [ ] | insetPadding | EdgeInsets? | getValue | inset-padding |
| [ ] | clipBehavior | Clip? | getValue | clip-behavior |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | alignment | AlignmentGeometry? | getValue | alignment |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | scrollable | bool | getBool | scrollable |

#### [ ] | BottomSheet | material/widgets/bottom_sheet.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | animationController | AnimationController? | getValue | animation-controller |
| [ ] | enableDrag | bool | getBool | enable-drag |
| [ ] | showDragHandle | bool? | getBool | show-drag-handle |
| [ ] | dragHandleColor | Color? | getColor | drag-handle-color |
| [ ] | dragHandleSize | Size? | getValue | drag-handle-size |
| [ ] | onDragStart | BottomSheetDragStartHandler? | getValue | on-drag-start |
| [ ] | onDragEnd | BottomSheetDragEndHandler? | getValue | on-drag-end |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | clipBehavior | Clip? | getValue | clip-behavior |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | onClosing | VoidCallback | getVoidCallback | on-closing |
| [ ] | builder | WidgetBuilder | getValue | builder |

#### [ ] | Card | material/widgets/card.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | color | Color? | getColor | color |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | borderOnForeground | bool | getBool | border-on-foreground |
| [ ] | margin | EdgeInsetsGeometry? | getValue | margin |
| [ ] | clipBehavior | Clip? | getValue | clip-behavior |
| [ ] | child | Widget? | properties.child | - |
| [ ] | semanticContainer | bool | getBool | semantic-container |

#### [ ] | Divider | material/widgets/divider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | height | double? | getDouble | height |
| [ ] | thickness | double? | getDouble | thickness |
| [ ] | indent | double? | getDouble | indent |
| [ ] | endIndent | double? | getDouble | end-indent |
| [ ] | color | Color? | getColor | color |
| [ ] | radius | BorderRadiusGeometry? | getValue | radius |

#### [ ] | ListTile | material/widgets/list_tile.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | title | Widget? | getWidget | title |
| [ ] | subtitle | Widget? | getWidget | subtitle |
| [ ] | trailing | Widget? | getWidget | trailing |
| [ ] | isThreeLine | bool? | getBool | is-three-line |
| [ ] | dense | bool? | getBool | dense |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | style | ListTileStyle? | getValue | style |
| [ ] | selectedColor | Color? | getColor | selected-color |
| [ ] | iconColor | Color? | getColor | icon-color |
| [ ] | textColor | Color? | getColor | text-color |
| [ ] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [ ] | subtitleTextStyle | TextStyle? | getValue | subtitle-text-style |
| [ ] | leadingAndTrailingTextStyle | TextStyle? | getValue | leading-and-trailing-text-style |
| [ ] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [ ] | enabled | bool | getBool | enabled |
| [ ] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [ ] | onLongPress | GestureLongPressCallback? | getValue | on-long-press |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | selected | bool | getBool | selected |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | tileColor | Color? | getColor | tile-color |
| [ ] | selectedTileColor | Color? | getColor | selected-tile-color |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | horizontalTitleGap | double? | getDouble | horizontal-title-gap |
| [ ] | minVerticalPadding | double? | getDouble | min-vertical-padding |
| [ ] | minLeadingWidth | double? | getDouble | min-leading-width |
| [ ] | minTileHeight | double? | getDouble | min-tile-height |
| [ ] | titleAlignment | ListTileTitleAlignment? | getValue | title-alignment |
| [ ] | internalAddSemanticForOnTap | bool | getBool | internal-add-semantic-for-on-tap |
| [ ] | statesController | MaterialStatesController? | getValue | states-controller |

#### [ ] | BottomAppBar | material/widgets/bottom_app_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | color | Color? | getColor | color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shape | NotchedShape? | getValue | shape |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | notchMargin | double | getDouble | notch-margin |
| [ ] | child | Widget? | properties.child | - |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | height | double? | getDouble | height |

#### [ ] | NavigationBar | material/widgets/navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | animationDuration | Duration? | getValue | animation-duration |
| [ ] | selectedIndex | int | getInt | selected-index |
| [ ] | destinations | List<Widget> | getWidgets | destinations |
| [ ] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [ ] | height | double? | getDouble | height |
| [ ] | labelBehavior | NavigationDestinationLabelBehavior? | getValue | label-behavior |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | labelTextStyle | WidgetStateProperty<TextStyle?>? | getValue | label-text-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | maintainBottomViewPadding | bool | getBool | maintain-bottom-view-padding |

#### [ ] | NavigationDrawer | material/widgets/navigation_drawer.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | children | List<Widget> | properties.children | - |
| [ ] | header | Widget? | getWidget | header |
| [ ] | footer | Widget? | getWidget | footer |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [ ] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [ ] | selectedIndex | int? | getInt | selected-index |
| [ ] | tilePadding | EdgeInsetsGeometry | getValue | tile-padding |

#### [ ] | NavigationRail | material/widgets/navigation_rail.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | extended | bool | getBool | extended |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | trailing | Widget? | getWidget | trailing |
| [ ] | destinations | List<NavigationRailDestination> | getValue | destinations |
| [ ] | selectedIndex | int? | getInt | selected-index |
| [ ] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | groupAlignment | double? | getDouble | group-alignment |
| [ ] | labelType | NavigationRailLabelType? | getValue | label-type |
| [ ] | unselectedLabelTextStyle | TextStyle? | getValue | unselected-label-text-style |
| [ ] | selectedLabelTextStyle | TextStyle? | getValue | selected-label-text-style |
| [ ] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [ ] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [ ] | minWidth | double? | getDouble | min-width |
| [ ] | minExtendedWidth | double? | getDouble | min-extended-width |
| [ ] | useIndicator | bool? | getBool | use-indicator |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [ ] | leadingAtTop | bool | getBool | leading-at-top |
| [ ] | trailingAtBottom | bool | getBool | trailing-at-bottom |
| [ ] | scrollable | bool | getBool | scrollable |

#### [ ] | TabBar | material/widgets/tab_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | tabs | List<Widget> | getWidgets | tabs |
| [ ] | controller | TabController? | getValue | controller |
| [ ] | isScrollable | bool | getBool | is-scrollable |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | automaticIndicatorColorAdjustment | bool | getBool | automatic-indicator-color-adjustment |
| [ ] | indicatorWeight | double | getDouble | indicator-weight |
| [ ] | indicatorPadding | EdgeInsetsGeometry | getValue | indicator-padding |
| [ ] | indicator | Decoration? | getValue | indicator |
| [ ] | indicatorSize | TabBarIndicatorSize? | getValue | indicator-size |
| [ ] | dividerColor | Color? | getColor | divider-color |
| [ ] | dividerHeight | double? | getDouble | divider-height |
| [ ] | labelColor | Color? | getColor | label-color |
| [ ] | labelStyle | TextStyle? | getValue | label-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | unselectedLabelColor | Color? | getColor | unselected-label-color |
| [ ] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | onHover | TabValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | TabValueChanged<bool>? | getValue | on-focus-change |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | splashFactory | InteractiveInkFeatureFactory? | getValue | splash-factory |
| [ ] | splashBorderRadius | BorderRadius? | getValue | splash-border-radius |
| [ ] | tabAlignment | TabAlignment? | getValue | tab-alignment |
| [ ] | textScaler | TextScaler? | getValue | text-scaler |
| [ ] | indicatorAnimation | TabIndicatorAnimation? | getValue | indicator-animation |

#### [ ] | TextField | material/widgets/text_field.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | groupId | Object | getValue | group-id |
| [ ] | controller | TextEditingController? | getValue | controller |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | undoController | UndoHistoryController? | getValue | undo-controller |
| [ ] | decoration | InputDecoration? | getValue | decoration |
| [ ] | keyboardType | TextInputType? | getValue | keyboard-type |
| [ ] | textInputAction | TextInputAction? | getValue | text-input-action |
| [ ] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | strutStyle | StrutStyle? | getValue | strut-style |
| [ ] | textAlign | TextAlign | getValue | text-align |
| [ ] | textAlignVertical | TextAlignVertical? | getValue | text-align-vertical |
| [ ] | textDirection | TextDirection? | getValue | text-direction |
| [ ] | readOnly | bool | getBool | read-only |
| [ ] | toolbarOptions | ToolbarOptions? | getValue | toolbar-options |
| [ ] | showCursor | bool? | getBool | show-cursor |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | statesController | MaterialStatesController? | getValue | states-controller |
| [ ] | obscuringCharacter | String | getString | obscuring-character |
| [ ] | obscureText | bool | getBool | obscure-text |
| [ ] | autocorrect | bool? | getBool | autocorrect |
| [ ] | smartDashesType | SmartDashesType? | getValue | smart-dashes-type |
| [ ] | smartQuotesType | SmartQuotesType? | getValue | smart-quotes-type |
| [ ] | enableSuggestions | bool | getBool | enable-suggestions |
| [ ] | maxLines | int? | getInt | max-lines |
| [ ] | minLines | int? | getInt | min-lines |
| [ ] | expands | bool | getBool | expands |
| [ ] | maxLength | int? | getInt | max-length |
| [ ] | maxLengthEnforcement | MaxLengthEnforcement? | getValue | max-length-enforcement |
| [ ] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [ ] | onEditingComplete | VoidCallback? | getVoidCallback | on-editing-complete |
| [ ] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [ ] | onAppPrivateCommand | AppPrivateCommandCallback? | getVoidCallback | on-app-private-command |
| [ ] | inputFormatters | List<TextInputFormatter>? | getValue | input-formatters |
| [ ] | enabled | bool? | getBool | enabled |
| [ ] | ignorePointers | bool? | getBool | ignore-pointers |
| [ ] | cursorWidth | double | getDouble | cursor-width |
| [ ] | cursorHeight | double? | getDouble | cursor-height |
| [ ] | cursorRadius | Radius? | getValue | cursor-radius |
| [ ] | cursorOpacityAnimates | bool? | getBool | cursor-opacity-animates |
| [ ] | cursorColor | Color? | getColor | cursor-color |
| [ ] | cursorErrorColor | Color? | getColor | cursor-error-color |
| [ ] | selectionHeightStyle | BoxHeightStyle? | getValue | selection-height-style |
| [ ] | selectionWidthStyle | BoxWidthStyle? | getValue | selection-width-style |
| [ ] | keyboardAppearance | Brightness? | getValue | keyboard-appearance |
| [ ] | scrollPadding | EdgeInsets | getValue | scroll-padding |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | enableInteractiveSelection | bool? | getBool | enable-interactive-selection |
| [ ] | selectAllOnFocus | bool? | getBool | select-all-on-focus |
| [ ] | selectionControls | TextSelectionControls? | getValue | selection-controls |
| [ ] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [ ] | onTapAlwaysCalled | bool | getBool | on-tap-always-called |
| [ ] | onTapOutside | TapRegionCallback? | getVoidCallback | on-tap-outside |
| [ ] | onTapUpOutside | TapRegionUpCallback? | getVoidCallback | on-tap-up-outside |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | buildCounter | InputCounterWidgetBuilder? | getValue | build-counter |
| [ ] | scrollController | ScrollController? | getValue | scroll-controller |
| [ ] | scrollPhysics | ScrollPhysics? | getValue | scroll-physics |
| [ ] | autofillHints | Iterable<String>? | getValue | autofill-hints |
| [ ] | contentInsertionConfiguration | ContentInsertionConfiguration? | getValue | content-insertion-configuration |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | scribbleEnabled | bool | getBool | scribble-enabled |
| [ ] | stylusHandwritingEnabled | bool | getBool | stylus-handwriting-enabled |
| [ ] | enableIMEPersonalizedLearning | bool | getBool | enable-ime-personalized-learning |
| [ ] | contextMenuBuilder | EditableTextContextMenuBuilder? | getValue | context-menu-builder |
| [ ] | canRequestFocus | bool | getBool | can-request-focus |
| [ ] | spellCheckConfiguration | SpellCheckConfiguration? | getValue | spell-check-configuration |
| [ ] | magnifierConfiguration | TextMagnifierConfiguration? | getValue | magnifier-configuration |
| [ ] | hintLocales | List<Locale>? | getValue | hint-locales |

#### [ ] | Checkbox | material/widgets/checkbox.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | bool? | getBool | value |
| [ ] | tristate | bool | getBool | tristate |
| [ ] | onChanged | ValueChanged<bool?>? | getValue | on-changed |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [ ] | checkColor | Color? | getColor | check-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | isError | bool | getBool | is-error |
| [ ] | semanticLabel | String? | getString | semantic-label |

#### [ ] | Chip | material/widgets/chip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | avatar | Widget? | getWidget | avatar |
| [ ] | label | Widget | properties.child | - |
| [ ] | labelStyle | TextStyle? | getValue | label-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | deleteIcon | Widget? | getWidget | delete-icon |
| [ ] | onDeleted | VoidCallback? | getVoidCallback | on-deleted |
| [ ] | deleteIconColor | Color? | getColor | delete-icon-color |
| [ ] | deleteButtonTooltipMessage | String? | getString | delete-button-tooltip-message |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | color | WidgetStateProperty<Color?>? | getValue | color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | iconTheme | IconThemeData? | getValue | icon-theme |
| [ ] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |
| [ ] | deleteIconBoxConstraints | BoxConstraints? | getValue | delete-icon-box-constraints |
| [ ] | chipAnimationStyle | ChipAnimationStyle? | getValue | chip-animation-style |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |

#### [ ] | DatePickerDialog | material/widgets/date_picker_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | initialDate | DateTime? | getValue | initial-date |
| [ ] | firstDate | DateTime | getValue | first-date |
| [ ] | lastDate | DateTime | getValue | last-date |
| [ ] | currentDate | DateTime? | getValue | current-date |
| [ ] | initialEntryMode | DatePickerEntryMode | getValue | initial-entry-mode |
| [ ] | selectableDayPredicate | SelectableDayPredicate? | getValue | selectable-day-predicate |
| [ ] | cancelText | String? | getString | cancel-text |
| [ ] | confirmText | String? | getString | confirm-text |
| [ ] | helpText | String? | getString | help-text |
| [ ] | errorFormatText | String? | getString | error-format-text |
| [ ] | errorInvalidText | String? | getString | error-invalid-text |
| [ ] | fieldHintText | String? | getString | field-hint-text |
| [ ] | fieldLabelText | String? | getString | field-label-text |
| [ ] | keyboardType | TextInputType? | getValue | keyboard-type |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | onDatePickerModeChange | ValueChanged<DatePickerEntryMode>? | getValue | on-date-picker-mode-change |
| [ ] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [ ] | switchToCalendarEntryModeIcon | Icon? | getValue | switch-to-calendar-entry-mode-icon |
| [ ] | insetPadding | EdgeInsets | getValue | inset-padding |
| [ ] | calendarDelegate | CalendarDelegate<DateTime> | getValue | calendar-delegate |

#### [ ] | MenuAnchor | material/widgets/menu_anchor.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | controller | MenuController? | getValue | controller |
| [ ] | childFocusNode | FocusNode? | getValue | child-focus-node |
| [ ] | style | MenuStyle? | getValue | style |
| [ ] | alignmentOffset | Offset | getValue | alignment-offset |
| [ ] | reservedPadding | EdgeInsetsGeometry? | getValue | reserved-padding |
| [ ] | layerLink | LayerLink? | getValue | layer-link |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | anchorTapClosesMenu | bool | getBool | anchor-tap-closes-menu |
| [ ] | consumeOutsideTap | bool | getBool | consume-outside-tap |
| [ ] | onOpen | VoidCallback? | getVoidCallback | on-open |
| [ ] | onClose | VoidCallback? | getVoidCallback | on-close |
| [ ] | crossAxisUnconstrained | bool | getBool | cross-axis-unconstrained |
| [ ] | useRootOverlay | bool | getBool | use-root-overlay |
| [ ] | menuChildren | List<Widget> | getWidgets | menu-children |
| [ ] | builder | MenuAnchorChildBuilder? | getValue | builder |
| [ ] | child | Widget? | properties.child | - |

#### [ ] | Radio | material/widgets/radio.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | T | getValue | value |
| [ ] | groupValue | T? | getValue | group-value |
| [ ] | onChanged | ValueChanged<T?>? | getValue | on-changed |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | toggleable | bool | getBool | toggleable |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | enabled | bool? | getBool | enabled |
| [ ] | groupRegistry | RadioGroupRegistry<T>? | getValue | group-registry |
| [ ] | backgroundColor | WidgetStateProperty<Color?>? | getValue | background-color |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | innerRadius | WidgetStateProperty<double?>? | getValue | inner-radius |

#### [ ] | Slider | material/widgets/slider.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | double | getDouble | value |
| [ ] | secondaryTrackValue | double? | getDouble | secondary-track-value |
| [ ] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [ ] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [ ] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [ ] | min | double | getDouble | min |
| [ ] | max | double | getDouble | max |
| [ ] | divisions | int? | getInt | divisions |
| [ ] | label | String? | getString | label |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | inactiveColor | Color? | getColor | inactive-color |
| [ ] | secondaryActiveColor | Color? | getColor | secondary-active-color |
| [ ] | thumbColor | Color? | getColor | thumb-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | semanticFormatterCallback | SemanticFormatterCallback? | getValue | semantic-formatter-callback |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | allowedInteraction | SliderInteraction? | getValue | allowed-interaction |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | year2023 | bool? | getBool | year2023 |

#### [ ] | Switch | material/widgets/switch.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | bool | getBool | value |
| [ ] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | activeThumbColor | Color? | getColor | active-thumb-color |
| [ ] | activeTrackColor | Color? | getColor | active-track-color |
| [ ] | inactiveThumbColor | Color? | getColor | inactive-thumb-color |
| [ ] | inactiveTrackColor | Color? | getColor | inactive-track-color |
| [ ] | activeThumbImage | ImageProvider<Object>? | getValue | active-thumb-image |
| [ ] | onActiveThumbImageError | ImageErrorListener? | getValue | on-active-thumb-image-error |
| [ ] | inactiveThumbImage | ImageProvider<Object>? | getValue | inactive-thumb-image |
| [ ] | onInactiveThumbImageError | ImageErrorListener? | getValue | on-inactive-thumb-image-error |
| [ ] | thumbColor | WidgetStateProperty<Color?>? | getValue | thumb-color |
| [ ] | trackColor | WidgetStateProperty<Color?>? | getValue | track-color |
| [ ] | trackOutlineColor | WidgetStateProperty<Color?>? | getValue | track-outline-color |
| [ ] | trackOutlineWidth | WidgetStateProperty<double?>? | getValue | track-outline-width |
| [ ] | thumbIcon | WidgetStateProperty<Icon?>? | getValue | thumb-icon |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |

#### [ ] | TimePickerDialog | material/widgets/time_picker_dialog.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | initialTime | TimeOfDay | getValue | initial-time |
| [ ] | cancelText | String? | getString | cancel-text |
| [ ] | confirmText | String? | getString | confirm-text |
| [ ] | helpText | String? | getString | help-text |
| [ ] | errorInvalidText | String? | getString | error-invalid-text |
| [ ] | hourLabelText | String? | getString | hour-label-text |
| [ ] | minuteLabelText | String? | getString | minute-label-text |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | initialEntryMode | TimePickerEntryMode | getValue | initial-entry-mode |
| [ ] | orientation | Orientation? | getValue | orientation |
| [ ] | onEntryModeChanged | EntryModeChangeCallback? | getValue | on-entry-mode-changed |
| [ ] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [ ] | switchToTimerEntryModeIcon | Icon? | getValue | switch-to-timer-entry-mode-icon |
| [ ] | emptyInitialInput | bool | getBool | empty-initial-input |

#### [ ] | Drawer | material/widgets/drawer.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | width | double? | getDouble | width |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | semanticLabel | String? | getString | semantic-label |
| [ ] | child | Widget? | properties.child | - |

#### [ ] | BottomNavigationBar | material/widgets/bottom_navigation_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | items | List<BottomNavigationBarItem> | getValue | items |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | currentIndex | int | getInt | current-index |
| [ ] | elevation | double | getDouble | elevation |
| [ ] | type | BottomNavigationBarType? | getValue | type |
| [ ] | fixedColor | Color? | getColor | fixed-color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | selectedItemColor | Color? | getColor | selected-item-color |
| [ ] | unselectedItemColor | Color? | getColor | unselected-item-color |
| [ ] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [ ] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [ ] | selectedLabelStyle | TextStyle? | getValue | selected-label-style |
| [ ] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [ ] | selectedFontSize | double | getDouble | selected-font-size |
| [ ] | unselectedFontSize | double | getDouble | unselected-font-size |
| [ ] | showSelectedLabels | bool? | getBool | show-selected-labels |
| [ ] | showUnselectedLabels | bool? | getBool | show-unselected-labels |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | landscapeLayout | BottomNavigationBarLandscapeLayout? | getValue | landscape-layout |

#### [ ] | DropdownButton | material/widgets/dropdown_button.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | items | List<DropdownMenuItem>? | getValue | items |
| [ ] | selectedItemBuilder | DropdownButtonBuilder? | getValue | selected-item-builder |
| [ ] | value | T? | getValue | value |
| [ ] | hint | Widget? | getWidget | hint |
| [ ] | disabledHint | Widget? | getWidget | disabled-hint |
| [ ] | onChanged | ValueChanged? | getValue | on-changed |
| [ ] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [ ] | elevation | int | getInt | elevation |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | underline | Widget? | getWidget | underline |
| [ ] | icon | Widget? | getWidget | icon |
| [ ] | iconDisabledColor | Color? | getColor | icon-disabled-color |
| [ ] | iconEnabledColor | Color? | getColor | icon-enabled-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | isDense | bool | getBool | is-dense |
| [ ] | isExpanded | bool | getBool | is-expanded |
| [ ] | itemHeight | double? | getDouble | item-height |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | dropdownColor | Color? | getColor | color |
| [ ] | menuMaxHeight | double? | getDouble | menu-max-height |
| [ ] | enableFeedback | bool | getBool | enable-feedback |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | borderRadius | BorderRadius? | getValue | border-radius |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |

#### [ ] | CircularProgressIndicator | material/widgets/circular_progress_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | value | double? | getDouble | value |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | color | Color? | getColor | color |
| [ ] | strokeWidth | double? | getDouble | stroke-width |
| [ ] | strokeAlign | double? | getDouble | stroke-align |
| [ ] | strokeCap | StrokeCap? | getValue | stroke-cap |
| [ ] | semanticsLabel | String? | getString | semantics-label |
| [ ] | semanticsValue | String? | getString | semantics-value |

#### [ ] | RefreshIndicator | material/widgets/refresh_indicator.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | child | Widget | properties.child | - |
| [ ] | displacement | double | getDouble | displacement |
| [ ] | edgeOffset | double | getDouble | edge-offset |
| [ ] | onRefresh | RefreshCallback | getValue | on-refresh |
| [ ] | color | Color? | getColor | color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | notificationPredicate | ScrollNotificationPredicate | getValue | notification-predicate |
| [ ] | semanticsLabel | String? | getString | semantics-label |
| [ ] | semanticsValue | String? | getString | semantics-value |
| [ ] | strokeWidth | double | getDouble | stroke-width |
| [ ] | triggerMode | RefreshIndicatorTriggerMode | getValue | trigger-mode |

#### [ ] | SearchBar | material/widgets/search_bar.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | controller | TextEditingController? | getValue | controller |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | hintText | String? | getString | hint-text |
| [ ] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [ ] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [ ] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | elevation | WidgetStateProperty? | getValue | elevation |
| [ ] | overlayColor | WidgetStateProperty? | getValue | overlay-color |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | textStyle | TextStyle? | getValue | text-style |
| [ ] | hintStyle | TextStyle? | getValue | hint-style |
| [ ] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [ ] | keyboardType | TextInputType | getValue | keyboard-type |

#### [ ] | ActionChip | material/widgets/action_chip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | avatar | Widget? | getValue | avatar |
| [ ] | label | Widget | getValue | label |
| [ ] | labelStyle | TextStyle? | getValue | label-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | pressElevation | double? | getValue | press-elevation |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | disabledColor | Color? | getColor | disabled-color |
| [ ] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |

#### [ ] | TabBarView | material/widgets/tab_bar_view.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | children | List<Widget> | getWidgets | children |
| [ ] | controller | TabController? | getValue | controller |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | viewportFraction | double | getDouble | viewport-fraction |

#### [ ] | Tooltip | material/widgets/tooltip.dart
| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [ ] | key | Key? | properties.key | - |
| [ ] | message | String? | getString | tooltip-message |
| [ ] | richMessage | InlineSpan? | getValue | tooltip-rich-message |
| [ ] | padding | EdgeInsetsGeometry? | getValue | tooltip-padding |
| [ ] | margin | EdgeInsetsGeometry? | getValue | tooltip-margin |
| [ ] | verticalOffset | double? | getDouble | tooltip-vertical-offset |
| [ ] | preferBelow | bool? | getBool | tooltip-prefer-below |
| [ ] | excludeFromSemantics | bool? | getBool | tooltip-exclude-from-semantics |
| [ ] | decoration | Decoration? | getValue | tooltip-decoration |
| [ ] | textStyle | TextStyle? | getValue | tooltip-text-style |
| [ ] | textAlign | TextAlign? | getValue | tooltip-text-align |
| [ ] | waitDuration | Duration? | getValue | tooltip-wait-duration |
| [ ] | showDuration | Duration? | getValue | tooltip-show-duration |
| [ ] | triggerMode | TooltipTriggerMode? | getValue | tooltip-trigger-mode |
| [ ] | enableFeedback | bool? | getBool | tooltip-enable-feedback |
| [ ] | onTriggered | VoidCallback? | getVoidCallback | tooltip-on-triggered |
| [ ] | child | Widget? | properties.child | - |
