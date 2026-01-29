import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_button.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_activity_indicator.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_switch.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_picker.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_date_picker.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_text_field.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_action_sheet.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_alert_dialog.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_context_menu.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_scrollbar.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_checkbox.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_slider.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_search_text_field.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_timer_picker.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_app.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_segmented_control.dart';
import 'package:glue_flutter/src/lib/ui/cupertino/widgets/cupertino_tab_bar.dart';

final ModuleInfo uiCupertinoModule = nativeModule('ffi.ui', [
  // Cupertino widget functions
  ('cupertino-button', cupertinoButton),
  ('cupertino-activity-indicator', cupertinoActivityIndicator),
  ('cupertino-navigation-bar', cupertinoNavigationBar),
  ('cupertino-switch', cupertinoSwitch),
  ('cupertino-picker', cupertinoPicker),
  ('cupertino-date-picker', cupertinoDatePicker),
  ('cupertino-text-field', cupertinoTextField),
  ('cupertino-action-sheet', cupertinoActionSheet),
  ('cupertino-alert-dialog', cupertinoAlertDialog),
  ('cupertino-context-menu', cupertinoContextMenu),
  ('cupertino-scrollbar', cupertinoScrollbar),
  ('cupertino-checkbox', cupertinoCheckbox),
  ('cupertino-slider', cupertinoSlider),
  ('cupertino-search-text-field', cupertinoSearchTextField),
  ('cupertino-timer-picker', cupertinoTimerPicker),
  ('cupertino-app', cupertinoApp),
  ('cupertino-segmented-control', cupertinoSegmentedControl),
  ('cupertino-tab-bar', cupertinoTabBar),
]);
