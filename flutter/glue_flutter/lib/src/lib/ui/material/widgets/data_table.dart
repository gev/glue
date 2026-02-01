import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// DataTable widget function
/// Creates Flutter DataTable from Glue (data-table props) expressions
final Ir dataTable = IrNativeFunc(dataTableImpl);

/// DataTable implementation - takes properties object
Eval<Ir> dataTableImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDataTable(
    WidgetProperties(properties.unlock),
  ),
  _ => _createDataTable(WidgetProperties.empty()),
};

/// Create DataTable widget from properties
Eval<Ir> _createDataTable(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final dataTableWidget = DataTable(
      key: properties.key,
      columns: properties.getValues<DataColumn>('columns'),
      rows: properties.getValues<DataRow>('rows'),
      sortColumnIndex: properties.getInt('sort-column-index'),
      sortAscending: properties.getBool('sort-ascending') ?? true,
      onSelectAll: properties.getCallback<bool>('on-select-all')?.call(runtime),
      dataRowColor: properties.getValue<WidgetStateProperty<Color>>(
        'data-row-color',
      ),
      dataTextStyle: properties.getValue<TextStyle>('data-text-style'),
      headingRowColor: properties.getValue<WidgetStateProperty<Color>>(
        'heading-row-color',
      ),
      headingRowHeight: properties.getDouble('heading-row-height'),
      headingTextStyle: properties.getValue<TextStyle>('heading-text-style'),
      horizontalMargin: properties.getDouble('horizontal-margin'),
      columnSpacing: properties.getDouble('column-spacing'),
      showCheckboxColumn: properties.getBool('show-checkbox-column') ?? false,
      showBottomBorder: properties.getBool('show-bottom-border') ?? false,
      dividerThickness: properties.getDouble('divider-thickness'),
      checkboxHorizontalMargin: properties.getDouble(
        'checkbox-horizontal-margin',
      ),
      border: properties.getValue<TableBorder>('border'),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    );
    return IrNativeValue(Value(dataTableWidget));
  });
}
