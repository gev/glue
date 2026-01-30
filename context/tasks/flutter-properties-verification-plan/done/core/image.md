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
| [ ] | cacheWidth | int? | getInt | cache-width |
| [ ] | cacheHeight | int? | getInt | cache-height |
