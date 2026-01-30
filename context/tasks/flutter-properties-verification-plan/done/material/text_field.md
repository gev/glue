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
