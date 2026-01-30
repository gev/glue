### Text Widget [x]

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
