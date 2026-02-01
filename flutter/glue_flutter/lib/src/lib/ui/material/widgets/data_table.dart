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
      columns: properties.getValue<>('columns') ?? [],
      rows: properties.getValue<>('rows') ?? [],
      sortColumnIndex: properties.getInt('sort-column-index'),
      sortAscending: properties.getBool('sort-ascending') ?? true,
      onSelectAll: properties.getValue<>('on-select-all'),
      dataRowColor: properties.getValue<>('data-row-color'),
      dataTextStyle: properties.getValue<>('data-text-style'),
      headingRowColor: properties.getValue<>('heading-row-color'),
      headingRowHeight: properties.getDouble('heading-row-height'),
      headingTextStyle: properties.getValue<>('heading-text-style'),
      horizontalMargin: properties.getDouble('horizontal-margin'),
      columnSpacing: properties.getDouble('column-spacing'),
      showCheckboxColumn: properties.getBool('show-checkbox-column') ?? false,
      showBottomBorder: properties.getBool('show-bottom-border') ?? false,
      dividerThickness: properties.getDouble('divider-thickness'),
      checkboxHorizontalMargin: properties.getDouble(
        'checkbox-horizontal-margin',
      ),
      border: properties.getValue<>('border'),
      clipBehavior: properties.getValue<>('clip-behavior') ?? Clip.none,
    );
    return IrNativeValue(Value(dataTableWidget));
  });
}
