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
