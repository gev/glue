import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/text.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/button.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/container.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/column.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/row.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/center.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/icon.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/placeholder.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/image.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/app_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/scaffold.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/filled_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/outlined_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/text_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/card.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/list_tile.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/snack_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/text_field.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/floating_action_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/icon_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/checkbox.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/switch.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/linear_progress_indicator.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/badge.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/divider.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/radio.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/slider.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/alert_dialog.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/elevated_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/chip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/segmented_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/bottom_navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/drawer.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/date_picker_dialog.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/time_picker_dialog.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/navigation_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/tab_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/menu_anchor.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/expansion_tile.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/data_table.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/tooltip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/popup_menu_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/dropdown_button.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/refresh_indicator.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/circular_progress_indicator.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/stepper.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/expansion_panel_list.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/tab_bar_view.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/bottom_sheet.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/search_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/search_anchor.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/input_chip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/filter_chip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/choice_chip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/action_chip.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/bottom_app_bar.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/navigation_drawer.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/drawer_header.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/user_accounts_drawer_header.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/list_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/grid_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/single_child_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/custom_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_list.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_grid.dart';
import 'package:glue_flutter/src/lib/ui/material/widgets/navigation_rail.dart';
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
