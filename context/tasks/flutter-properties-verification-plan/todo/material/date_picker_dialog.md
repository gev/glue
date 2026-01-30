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
