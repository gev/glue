# Material Widgets Documentation

This folder contains local documentation for Flutter Material widgets, extracted from the official Flutter documentation for offline access and Glue UI specification development.

Source documentation:
- https://docs.flutter.dev/ui/widgets/material
- https://api.flutter.dev/flutter/material/material-library.html

## Overview

Material widgets implement the Material 3 design specification, which is the default design language of Flutter (since Flutter 3.16). These widgets enable the design and building of beautiful, usable apps that adapt to any platform.

## Widget Categories

Material widgets are organized into the following functional categories:

### [Actions](actions.md)
Widgets for initiating actions and interactions:
- Common buttons
- FloatingActionButton
- Extended FloatingActionButton  
- IconButton
- SegmentedButton

### [Communication](communication.md)
Widgets for conveying information and status:
- Badge
- LinearProgressIndicator
- SnackBar

### [Containment](containment.md)
Widgets for organizing and grouping content:
- AlertDialog
- Bottom sheet
- Card
- Divider
- ListTile

### [Navigation](navigation.md)
Widgets for app navigation and screen transitions:
- AppBar
- Bottom app bar
- NavigationBar
- NavigationDrawer
- NavigationRail
- TabBar

### [Selection](selection.md)
Widgets for user input and data selection:
- Checkbox
- Chip
- DatePicker
- Menu
- Radio
- Slider
- Switch
- TimePicker

### [Text inputs](text_inputs.md)
Widgets for text entry and forms:
- TextField

## Documentation Maintenance

This documentation was extracted from Flutter's official API documentation and is focused on constructor information for Glue UI specification development.

To update this documentation:
```bash
# Fetch latest widget API documentation
# Extract constructor signatures and parameter details
# Update individual section files with new information
```

## Glue UI Integration Notes

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material widgets programmatically.
