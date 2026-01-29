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
  final dataTableWidget = DataTable(
    columns: properties.dataTableColumns ?? [],
    rows: properties.dataTableRows ?? [],
    sortColumnIndex: properties.dataTableSortColumnIndex,
    sortAscending: properties.dataTableSortAscending,
    onSelectAll: properties.dataTableOnSelectAll,
    dataRowColor: properties.dataTableDataRowColor,
    dataRowHeight: properties.dataTableDataRowHeight,
    dataTextStyle: properties.dataTableDataTextStyle,
    headingRowColor: properties.dataTableHeadingRowColor,
    headingRowHeight: properties.dataTableHeadingRowHeight,
    headingTextStyle: properties.dataTableHeadingTextStyle,
    horizontalMargin: properties.dataTableHorizontalMargin,
    columnSpacing: properties.dataTableColumnSpacing,
    showCheckboxColumn: properties.dataTableShowCheckboxColumn,
    showBottomBorder: properties.dataTableShowBottomBorder,
    dividerThickness: properties.dataTableDividerThickness,
    checkboxHorizontalMargin: properties.dataTableCheckboxHorizontalMargin,
    border: properties.dataTableBorder,
    clipBehavior: properties.dataTableClipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(dataTableWidget)));
}
