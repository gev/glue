# 🚀 Glue

> The universal metadata language that turns data into executable logic - formal Lisp syntax meets evaluation semantics for metadata-driven applications.

[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

## ✨ What Makes Glue Special?

**Not just another data format** - Glue is a **formal, evaluatable language** that solves the JSON maintenance nightmare:

### 🎯 **Formal Syntax, Strong Semantics**
JSON becomes unmaintainable mess
```json
{"type": "table", "columns": [...], "filters": [...]}
```

Glue provides formal structure + execution
```clojure
(table
  :columns (name age email)
  :data (fetch-users)
  :filters (search-filter sort-filter))
```

### 🔧 **Evaluation Semantics**
- **Executable metadata** - not just data, but runnable logic
- **Lexical scoping** - proper variable and function resolution
- **FFI integration** - seamless host language connectivity
- **Type safety** - runtime checking prevents runtime errors

### 🌍 **Universal Through Environment Injection**
Same Glue syntax works across domains via lexical environment:
- **Web UI**: `table` renders React/HTML components
- **Mobile UI**: `table` renders native components
- **IoT**: `device` controls hardware through injected APIs
- **Business Logic**: `process` executes domain operations

## 🎨 Key Applications

### 🖥️ **Metadata-Driven UI (MDUI)**
Eliminate frontend boilerplate - backend defines UI structure, frontend renders dynamically:
```clojure
;; Backend sends metadata, frontend renders UI automatically

(def register-form (lambda (props)
      (form
          :build (column ((text-field :label i18n.name :value props.name :on-change validate-name)
                          (text-field :label i18n.e-mail :value props.e-mail :on-change validate-e-mail)
                          (row ((button :label i18n.submit :type submit)
                                (button :label i18n.cancel :type cancel)))))
          :on-submit props.submit
          :on-cancel props.cancel)))
          
```

### 🤖 **IoT Device Orchestration**
Device specs and automation logic in one formal language:
```clojure
;; Define device capabilities 
(device 
    :model mix
    :power-voltage v12
    :ports ((relay 6)
            (dimmer 6)
            (one-wire 1)))
```

### 🔄 **Business Logic DSLs**
Express complex workflows as executable metadata:
```clojure
;; Order processing pipeline
(pipeline "order-fulfillment"
  :steps (validate-order charge-payment ship-goods)
  :error-handling (refund-payment notify-customer))
```

## 🏗️ Architecture

```
Host Application (Any Language)
    ↓ Injects domain functions into Glue environment
Glue Interpreter
    ↓ Receives metadata in formal Lisp syntax
    ↓ Evaluates with injected host capabilities
    ↓ Returns structured results to host
```

## 📦 Ecosystem

- **[`spec/`](spec/)**: Complete formal language specification
- **[`examples/`](examples/)**: Sample metadata applications
- **[`haskell/`](haskell/)**: Reference implementation
- **[`glue-ext/`](glue-ext/)**: VS Code syntax highlighting
- **[`context/`](context/)**: Development guidelines

## 🚀 Why Choose Glue?

### ✅ **Solves Real Problems**
- **No more JSON schema drift** - formal grammar prevents maintenance hell
- **Cross-platform consistency** - same metadata works everywhere
- **Backend autonomy** - developers define UI/business logic directly
- **Type safety** - catch errors at runtime, not in production

### 🎯 **Perfect For**
- **Complex admin panels** - eliminate repetitive CRUD interfaces
- **IoT platforms** - unified device specification and control
- **API gateways** - metadata-driven request processing
- **Cross-platform apps** - single source of truth for UI and logic

### ⚡ **Performance & Safety**
- **Minimal runtime footprint** - tree-walking interpreter optimized for metadata
- **Memory safe** - no pointers, no undefined behavior
- **Fast evaluation** - compiled to efficient intermediate representation

## 🛠️ Getting Started

1. **Read the Overview** → [`spec/language-overview.md`](spec/language-overview.md)
2. **Try Examples** → [`examples/`](examples/) directory
3. **Run Reference Implementation** → [`haskell/`](haskell/) directory

## 🤝 Contributing

Glue welcomes contributions to specifications, implementations, and tooling. See [`context/spec-creation-plan.md`](context/spec-creation-plan.md) for guidelines.

## 📄 License

BSD 3-Clause License - see LICENSE file for details.

---

**Glue: Formal metadata, executable logic, universal applications.** ✨
