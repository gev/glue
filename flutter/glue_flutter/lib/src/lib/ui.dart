import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/lib/ui/widgets/text.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/container.dart';
import 'package:glue_flutter/src/lib/ui/widgets/column.dart';
import 'package:glue_flutter/src/lib/ui/widgets/row.dart';
import 'package:glue_flutter/src/lib/ui/widgets/center.dart';
import 'package:glue_flutter/src/lib/ui/widgets/icon.dart';
import 'package:glue_flutter/src/lib/ui/widgets/placeholder.dart';
import 'package:glue_flutter/src/lib/ui/widgets/image.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/app_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/scaffold.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/filled_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/outlined_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/text_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/card.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/list_tile.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/snack_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/text_field.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/floating_action_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/icon_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/checkbox.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/switch.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/linear_progress_indicator.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/badge.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/divider.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/radio.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/slider.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/alert_dialog.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/elevated_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/chip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/segmented_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/bottom_navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/drawer.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/date_picker_dialog.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/time_picker_dialog.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/tab_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/menu_anchor.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/expansion_tile.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/data_table.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/tooltip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/popup_menu_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/dropdown_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/refresh_indicator.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/circular_progress_indicator.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/stepper.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/expansion_panel_list.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/tab_bar_view.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/bottom_sheet.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/search_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/search_anchor.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/input_chip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/filter_chip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/choice_chip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/action_chip.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/bottom_app_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/navigation_drawer.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/drawer_header.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/user_accounts_drawer_header.dart';
import 'package:glue_flutter/src/lib/ui/widgets/list_view.dart';
import 'package:glue_flutter/src/lib/ui/widgets/grid_view.dart';
import 'package:glue_flutter/src/lib/ui/widgets/single_child_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/widgets/custom_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/widgets/sliver_list.dart';
import 'package:glue_flutter/src/lib/ui/widgets/sliver_grid.dart';
import 'package:glue_flutter/src/lib/ui/widgets/material/navigation_rail.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_button.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_activity_indicator.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_switch.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_picker.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_date_picker.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_text_field.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_action_sheet.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_alert_dialog.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_context_menu.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_scrollbar.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_checkbox.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_slider.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_search_text_field.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_timer_picker.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_app.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_segmented_control.dart';
import 'package:glue_flutter/src/lib/ui/widgets/cupertino/cupertino_tab_bar.dart';
import 'package:glue_flutter/src/lib/ui/styles/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/styles/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/styles/main_axis_size.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_align.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_direction.dart';
import 'package:glue_flutter/src/lib/ui/styles/vertical_direction.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_baseline.dart';
import 'package:glue_flutter/src/lib/ui/styles/clip.dart';
import 'package:glue_flutter/src/lib/ui/styles/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/styles/colors.dart'; // Named colors object
import 'package:glue_flutter/src/lib/ui/styles/rgb.dart'; // RGB function
import 'package:glue_flutter/src/lib/ui/styles/rgba.dart'; // RGBA function
import 'package:glue_flutter/src/lib/ui/styles/padding_all.dart'; // padding-all function
import 'package:glue_flutter/src/lib/ui/styles/padding_symmetric.dart'; // padding-symmetric function
import 'package:glue_flutter/src/lib/ui/styles/padding_only.dart'; // padding-only function
import 'package:glue_flutter/src/lib/ui/styles/padding_directional.dart'; // padding-directional function
import 'package:glue_flutter/src/lib/ui/styles/flutter_logo_style.dart';
import 'package:glue_flutter/src/lib/ui/styles/box_fit.dart';
import 'package:glue_flutter/src/lib/ui/styles/image_repeat.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_overflow.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_width_basis.dart';
import 'package:glue_flutter/src/lib/ui/styles/filter_quality.dart';
import 'package:glue_flutter/src/lib/ui/styles/drag_start_behavior.dart';
import 'package:glue_flutter/src/lib/ui/styles/axis.dart';
import 'package:glue_flutter/src/lib/ui/styles/brightness.dart';
import 'package:glue_flutter/src/lib/ui/styles/floating_action_button_location.dart';
import 'package:glue_flutter/src/lib/ui/styles/theme_dark.dart';
import 'package:glue_flutter/src/lib/ui/styles/theme_light.dart';
import 'package:glue_flutter/src/lib/ui/styles/theme.dart';
import 'package:glue_flutter/src/lib/ui/styles/color_scheme_light.dart';
import 'package:glue_flutter/src/lib/ui/styles/color_scheme_dark.dart';
import 'package:glue_flutter/src/lib/ui/styles/text_theme.dart';
import 'package:glue_flutter/src/lib/ui/styles/system_brightness.dart';

// Note: All exports are handled through the Glue ModuleInfo below.
// No Dart re-exports to keep the module boundary clean.

/// UI module - Flutter implementation of framework-agnostic UI API
/// Provides concrete Flutter rendering for abstract UI specifications

/// The ui module containing all UI functions and enum objects
/// Implements the framework-agnostic UI API with Flutter widgets and enum unions
final ModuleInfo uiModule = nativeModule('ffi.ui', [
  // Core widget functions
  ('text', text),
  ('button', button),
  ('container', container),
  ('column', column),
  ('row', row),
  ('center', center),
  ('icon', icon),
  ('placeholder', placeholder),
  ('image', image),
  ('app-bar', appBar),
  ('scaffold', scaffold),

  // Material widgets
  ('filled-button', filledButton),
  ('outlined-button', outlinedButton),
  ('text-button', textButton),
  ('card', card),
  ('list-tile', listTile),
  ('snack-bar', snackBar),
  ('text-field', textField),
  ('floating-action-button', floatingActionButton),
  ('icon-button', iconButton),
  ('checkbox', checkbox),
  ('switch', switchWidget),
  ('linear-progress-indicator', linearProgressIndicator),
  ('badge', badge),
  ('divider', divider),
  ('radio', radio),
  ('slider', slider),
  ('alert-dialog', alertDialog),
  ('elevated-button', elevatedButton),
  ('chip', chip),
  ('segmented-button', segmentedButton),
  ('bottom-navigation-bar', bottomNavigationBar),
  ('drawer', drawer),
  ('date-picker-dialog', datePickerDialog),
  ('time-picker-dialog', timePickerDialog),
  ('navigation-bar', navigationBar),
  ('tab-bar', tabBar),
  ('menu-anchor', menuAnchor),
  ('expansion-tile', expansionTile),
  ('data-table', dataTable),
  ('tooltip', tooltip),
  ('popup-menu-button', popupMenuButton),
  ('dropdown-button', dropdownButton),
  ('refresh-indicator', refreshIndicator),
  ('circular-progress-indicator', circularProgressIndicator),
  ('stepper', stepper),
  ('expansion-panel-list', expansionPanelList),
  ('tab-bar-view', tabBarView),
  ('bottom-sheet', bottomSheet),
  ('search-bar', searchBar),
  ('search-anchor', searchAnchor),
  ('input-chip', inputChip),
  ('filter-chip', filterChip),
  ('choice-chip', choiceChip),
  ('action-chip', actionChip),
  ('bottom-app-bar', bottomAppBar),
  ('navigation-drawer', navigationDrawer),
  ('drawer-header', drawerHeader),
  ('user-accounts-drawer-header', userAccountsDrawerHeader),
  ('list-view', listView),
  ('grid-view', gridView),
  ('single-child-scroll-view', singleChildScrollView),
  ('custom-scroll-view', customScrollView),
  ('sliver-list', sliverList),
  ('sliver-grid', sliverGrid),
  ('navigation-rail', navigationRail),

  // Cupertino widgets
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

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),

  // Padding creation functions
  ('padding-all', paddingAll),
  ('padding-symmetric', paddingSymmetric),
  ('padding-only', paddingOnly),
  ('padding-directional', paddingDirectional),

  // Enum union objects
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('main-axis-size', mainAxisSize),
  ('text-align', textAlign),
  ('text-direction', textDirection),
  ('vertical-direction', verticalDirection),
  ('text-baseline', textBaseline),
  ('clip', clip),
  ('font-weight', fontWeight),
  ('colors', colors),
  ('flutter-logo-style', flutterLogoStyle),
  ('box-fit', boxFit),
  ('image-repeat', imageRepeat),
  ('text-overflow', textOverflow),
  ('text-width-basis', textWidthBasis),
  ('filter-quality', filterQuality),
  ('drag-start-behavior', dragStartBehavior),
  ('axis', axis),
  ('floating-action-button-location', floatingActionButtonLocation),

  ('brightness', brightness),
  ('system-brightness', systemBrightness),

  // Theme functions and objects
  ('theme-dark', themeDark),
  ('theme-light', themeLight),
  ('theme', theme),
  ('color-scheme-light', colorSchemeLight),
  ('color-scheme-dark', colorSchemeDark),
  ('text-theme', textTheme),
]);
