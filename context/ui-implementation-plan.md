# Framework-Agnostic UI Module (`ffi.ui`)

## Overview

The `ffi.ui` module provides a framework-agnostic API for creating user interfaces in Glue. The same Glue code can produce UI components across different frameworks (Flutter, React, Vue, etc.) by using framework-specific implementations of the same module interface.

## Core Concept

**Framework-agnostic UI metadata** - Glue serves as a universal UI description language that can be compiled to different framework-specific implementations.

```clojure
;; Same Glue code works everywhere
(import "ffi.ui")

(container :children [
  (text "Hello World" :color "blue" :size 24)
  (button :label "Click Me" :on-tap handle-click)
])
```

## Module Interface

### Module Name
- **Import name**: `"ffi.ui"`
- **Purpose**: Universal UI component library
- **Framework implementations**: Flutter, React, Vue, Web Components, etc.

### Core Functions

#### Text Display (`text`)
Creates text display elements with optional styling.

```clojure
;; Basic usage
(text "Hello World")

;; With styling
(text "Styled Text"
  :color "blue"
  :size 18
  :weight "bold"
  :align "center")
```

**Parameters:**
- `content` (required): Text content to display
- `:color`: Text color (named colors, hex codes)
- `:size`: Font size in points
- `:weight`: Font weight ("normal", "bold", etc.)
- `:align`: Text alignment ("left", "center", "right")

#### Interactive Elements (`button`)
Creates interactive button components.

```clojure
;; Basic button
(button :label "Click Me")

;; Button with event handler
(button :label "Save"
  :on-tap save-handler
  :disabled false
  :variant "primary")
```

**Parameters:**
- `:label` (required): Button text
- `:on-tap`: Callback function for tap events
- `:on-press`: Callback function for press events
- `:disabled`: Boolean to disable interaction
- `:variant`: Visual style ("primary", "secondary", "outline")

#### Layout Containers (`container`)
Creates layout containers for organizing child components.

```clojure
;; Vertical layout (default)
(container :children [
  (text "Item 1")
  (text "Item 2")
])

;; Horizontal layout
(container :direction "horizontal"
  :children [...]
  :spacing 16
  :align "center")
```

**Parameters:**
- `:children` (required): List of child components
- `:direction`: Layout direction ("vertical", "horizontal")
- `:spacing`: Space between children
- `:align`: Child alignment ("start", "center", "end", "stretch")
- `:padding`: Container padding

## Framework Implementations

### Flutter Implementation
The initial implementation uses Flutter widgets:

```dart
// text -> Text widget
// button -> ElevatedButton widget
// container -> Column/Row widgets
```

### React Implementation (Future)
```jsx
// text -> <span> element
// button -> <button> element
// container -> <div> with flexbox
```

### Vue Implementation (Future)
```vue
<!-- text -> <span> component -->
<!-- button -> <button> component -->
<!-- container -> <div> with flex layout -->
```

## Design Principles

### 1. Framework Agnostic API
The Glue API remains consistent regardless of the target framework. Framework-specific features are abstracted behind common parameter names.

### 2. Progressive Enhancement
Basic components work everywhere, advanced features are framework-specific extensions.

### 3. Type Safety
Strong typing in Glue prevents runtime UI errors. Parameter validation ensures correct usage.

### 4. Performance Optimized
Each framework implementation is optimized for its platform's rendering model.

## Component Lifecycle

### Creation
Components are created through Glue function calls and return framework-specific widget objects.

### Updates
Glue's reactive evaluation model automatically updates UI when underlying data changes.

### Events
User interactions trigger Glue callback functions, maintaining the functional programming model.

## Styling Approach

### Unified Parameter System
Common styling properties work across frameworks:

```clojure
;; Colors work everywhere
:color "red"        ;; Named color
:color "#FF0000"    ;; Hex color
:color "rgb(255,0,0)" ;; RGB notation

;; Sizes are framework-adapted
:size 16            ;; Points on mobile, pixels on web
```

### Framework-Specific Extensions
Each implementation can add framework-specific parameters:

```clojure
;; Flutter-specific
(button :label "Flutter Button"
  :elevation 4
  :shape "rounded")

;; React-specific
(button :label "React Button"
  :class-name "btn-primary"
  :data-testid "submit-btn")
```

## Error Handling

### Validation Errors
- Missing required parameters
- Invalid parameter types
- Unknown parameter names
- Malformed callback functions

### Framework-Specific Errors
- Rendering failures
- Layout constraint violations
- Event handler exceptions

## Extensibility

### Custom Components
New components can be added to the module:

```clojure
;; Framework implementations define new functions
(icon :name "user" :size 24)
(card :title "Card Title" :children [...])
```

### Theming
Global theming through module configuration:

```clojure
;; Set global theme
(set-theme :primary-color "blue" :font-family "Roboto")

;; Components inherit theme automatically
(button :label "Themed Button") ;; Uses primary color
```

## Cross-Framework Compatibility

### Code Portability
The same Glue UI code runs on any supported framework without modification.

### Progressive Web Apps
Single Glue codebase can target mobile (Flutter), web (React), and desktop (Flutter/Electron).

### Component Libraries
Third-party component libraries can implement the `ffi.ui` interface for custom components.

## Implementation Architecture

### Module Structure
```
ffi.ui/
├── core/           # Common interfaces and types
├── flutter/        # Flutter implementation
├── react/          # React implementation
├── vue/            # Vue implementation
└── web/            # Web Components implementation
```

### Function Registration
Each framework registers its component implementations:

```dart
// Flutter registration
final ui = nativeModule('ffi.ui', [
  ('text', IrNative(NativeFunc(flutterText))),
  ('button', IrNative(NativeFunc(flutterButton))),
  ('container', IrNative(NativeFunc(flutterContainer))),
]);
```

## Future Extensions

### Advanced Components
- Form inputs and validation
- Navigation components
- Data display (tables, lists, charts)
- Media components (images, video)

### Animation System
- Declarative animations
- Transition effects
- State-based animations

### Accessibility
- Screen reader support
- Keyboard navigation
- Focus management

## Benefits

### Developer Experience
- **Single codebase** for multi-platform UI
- **Type-safe** UI development
- **Hot reload** during development
- **Consistent API** across platforms

### Business Value
- **Faster development** with unified UI language
- **Reduced maintenance** with single source of truth
- **Framework flexibility** for technology choices
- **Future-proof** UI architecture

## Conclusion

The `ffi.ui` module represents a paradigm shift in UI development - moving from framework-specific APIs to a universal, framework-agnostic UI description language. By leveraging Glue's formal semantics and evaluation model, UI development becomes more declarative, type-safe, and portable across platforms and frameworks.
