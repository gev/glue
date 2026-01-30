get the first task in the folder flutter-properties-verification-plan/todo

get every umarked widget/property in the task 

compare and verify widget source code!
source codes in the folder  @flutter/glue_flutter/lib/src/ui

IMPRTANT:  pay attention to add a `key` property!!!
fix source code of the incorrect properties!!!
strictly follow the tables!!! Dont look and dont analize errors in the code!!
IMPORTANT when task totally ccomplitted move it into folder
flutter-properties-verification-plan/done

IMPORTANT: mark properties as done!
IMPORTANT: mark widget as done!

commit!
dont push!

ADD KEY PROPERTY INTO EVERY WIDGET!!!!!
IMPORTANT: move task!
IMPORTANT: mark properties as done!
IMPORTANT: mark widget as done!

dont run build
dont run app
dont run test
dont run analize
dont run flutter### Property Access Methods by Type
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

- [x] TabBarView widget

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | children | List<Widget> | getWidgets | children |
| [x] | controller | TabController? | getValue | controller |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | viewportFraction | double | getDouble | viewport-fraction |
