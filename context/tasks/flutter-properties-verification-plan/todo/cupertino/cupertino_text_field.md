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
