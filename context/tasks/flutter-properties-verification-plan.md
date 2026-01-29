## Prerequisites: Read Flutter Widgets Documentation

Review the local Flutter widgets documentation:

- [Basic Widgets](../docs/flutter_widgets/basic_widgets.md)
- [Layout Widgets](../docs/flutter_widgets/layout_widgets.md)
- [Cupertino Widgets](../docs/flutter_widgets/cupertino_widgets.md)
- [Input Widgets](../docs/flutter_widgets/input_widgets.md)
- [Asset Widgets](../docs/flutter_widgets/asset_widgets.md)
- [Themes](../docs/flutter_widgets/themes.md)
- Material Widgets:
  - [Actions](../docs/flutter_widgets/material/actions.md)
  - [Communication](../docs/flutter_widgets/material/communication.md)
  - [Containment](../docs/flutter_widgets/material/containment.md)
  - [Navigation](../docs/flutter_widgets/material/navigation.md)
  - [Selection](../docs/flutter_widgets/material/selection.md)
  - [Text Inputs](../docs/flutter_widgets/material/text_inputs.md)

### Property Access Methods by Type
- **Direct Property Access**: `key`, `child`, `children`, `width`, `height`, `top`, `bottom`, `left`, `right`, `start`, `end`, `horizontal`, `vertical`
  - Access as: `properties.key`, `properties.child`, `properties.children`, etc.
- **bool**: `properties.getBool('property-name')`
- **Color**: `properties.getColor('property-name')`
- **double**: `properties.getDouble('property-name')`
- **int**: `properties.getInt('property-name')`
- **String**: `properties.getString('property-name')`
- **Widget**: `properties.getWidget('property-name')`
- **List<Widget>**: `properties.getWidgets('property-name')`
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
| [ ] | body | Widget? | getWidget | body |
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
| [ ] | focusNode | FocusNode? |
