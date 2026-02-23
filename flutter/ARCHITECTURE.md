# Architecture: Glue Application Browser

## The Big Idea

**Native mobile and desktop applications** built on the same principle as a web browser — but instead of HTML/CSS/JS interpreted by a browser, everything is **Glue** interpreted by a native Flutter runtime.

In a web browser the server delivers pages as executable programs (HTML + JS) and the browser renders them natively. Here, the server delivers UI and logic as Glue programs and the native app renders them as Flutter widgets. The app itself contains no application logic — it is a universal Glue runtime that happens to run natively on iOS, Android, macOS, Windows, and Linux.

The server owns all domain logic, UI definitions, and application state. The client exposes its native capabilities (navigation, platform UI, local state) back to the server through Glue.

Crucially, **both sides can call functions on the other**. The server can remotely invoke client-side capabilities (navigate to a screen, show a dialog, trigger a platform animation). The client can invoke server-side capabilities (fetch a resource, submit an action, query domain data). The WebSocket is a **bidirectional Glue RPC channel**.

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────────────────┐
│                              SERVER                                      │
│                                                                          │
│   Domain Environment: repositories, services, business rules             │
│   + client proxy functions: navigate, show-dialog, update-state, ...     │
│                                                                          │
│                      [ Glue Server Controller ]                          │
└────────────────────────────┬─────────────────────────────────────────────┘
                             │
                             │  WebSocket — Glue text in both directions
                             │  (get 'id) / (put 'id body)
                             │
┌────────────────────────────▼─────────────────────────────────────────────┐
│                              CLIENT                                      │
│                                                                          │
│                      [ Glue Client Controller ]                          │
│                                                                          │
│  Client Environment: navigation, UI actions, local state, store access   │
│  + server proxy functions: get-resource, submit-action, query, ...       │
│                                                                          │
│   ┌─────────────────┐         ┌───────────────────────────────────────┐  │
│   │  Persistent     │         │  Reactive In-Memory State Store       │  │
│   │  Cache          │────────▶│  StateNotifier<Ir> per resource ID    │  │
│   │  (Glue source)  │  eval   └──────────────┬────────────────────────┘  │
│   └─────────────────┘                        │ notify                    │
│                                   ┌──────────▼─────────────────────┐     │
│                                   │  Flutter UI Renderer           │     │
│                                   │  (Glue → native widgets)       │     │
│                                   └────────────────────────────────┘     │
└──────────────────────────────────────────────────────────────────────────┘
```

---

## Core Principle: Symmetric Glue RPC

Both sides **expose capabilities** as functions in their Glue environments, and both sides **invoke the other's capabilities** by sending Glue expressions over the WebSocket.

| Direction | What it enables |
|---|---|
| Client → Server | `(get 'id)` — request resources, submit actions, query domain data |
| Server → Client | `(put 'id body)` — push resources, trigger navigation, update state |

Neither side is purely passive. The server can drive the client at any time — navigate, update state, invoke any function the client has registered in its Glue environment.

---

## The Resource Store

The client maintains a single **keyed reactive store** of Glue values. Both state and screens live here, identified by ID. There is no distinction at the storage level — both are `Ir` values wrapped in `StateNotifier`.

| ID | Value | Role |
|---|---|---|
| `'user-profile` | `(:name "Alice" :age 30 ...)` | State |
| `'cart` | `((:id "p1" :qty 2) ...)` | State |
| `'notifications` | `(...)` | State |
| `'home-screen` | `(listen user-profile (\ (user) ...))` | Screen |
| `'cart-screen` | `(listen cart (\ (cart) ...))` | Screen |
| `'profile-screen` | `(listen user-profile (\ (user) ...))` | Screen |

### State Resources
Pure data delivered by the server. Stored as Glue values. When updated, all screens that listen to them re-render automatically.

### Screen Resources
Glue expressions that produce UI. A screen is **not a lambda over state** — it is a `listen` expression that subscribes to one or more state IDs and re-renders its body whenever those state values change:

```clojure
(listen user-profile
  (lambda (user)
    (column
      (text user.name)
      (text user.plan))))
```

- `listen` establishes the reactive subscription for the client's `Listenable` widget
- The inner `lambda` takes no arguments — it binds the current state value via `def` and accesses its properties with dot notation
- When the `StateNotifier` for `'user-profile` fires, the lambda is re-evaluated and the widget tree updates

This is analogous to reactive signals or `useSelector` in React, but expressed entirely in Glue and defined server-side.

### Compositional Screens: Sub-Resources

Screens are composed from smaller fragments using `lookup` + `listen`. `lookup` is the core storage primitive — given a quoted symbol ID, it returns a `StateNotifier` from the store. Its resolution order is:

1. **In-memory store** → return existing `StateNotifier` immediately
2. **Persistent cache** → evaluate the cached Glue source, populate the store, return notifier
3. **Not found** → register a placeholder `StateNotifier`, send `(get 'id)` to the server; when `(put 'id body)` arrives the client persists to cache, evaluates, and updates the notifier → all `listen` subscribers re-render automatically

```clojure
(def header-widget     (lookup 'header-widget))
(def user-stats-widget (lookup 'user-stats-widget))

(listen user-profile
  (lambda (user)
    (column
      (listen header-widget
        (lambda (header) (header ())))
      (listen user-stats-widget
        (lambda (user-stats)
          (user-stats user-profile))))))
```

| ID | Role |
|---|---|
| `'home-screen` | Screen (top-level) |
| `'header-widget` | Screen fragment (sub-resource) |
| `'user-stats-widget` | Screen fragment (sub-resource) |
| `'user-profile` | State |

- `lookup` is called once per resource — returns the `StateNotifier` and triggers a server fetch if needed
- `listen` attaches a reactive lambda to a `StateNotifier` — re-runs on every value change
- `read` reads the current value of the notifier inside a `listen` body
- `listen` can be nested — each sub-tree re-renders independently when its own state changes

This enables **incremental, compositional UI delivery**:
- Fragments are fetched lazily and cached; server updates re-render only the affected fragment
- Fragments can be shared across multiple parent screens
- Each `listen` node is an independent reactive boundary

---

## The Wire Protocol

Messages in both directions are Glue expressions sent as plain text over the WebSocket.

### Client → Server
```clojure
(get 'home-screen)              ;; request a resource
(get 'user-profile)             ;; request state data
```

### Server → Client
```clojure
(put 'user-profile (:name "Alice" :age 30 :plan "pro"))
(put 'home-screen  (listen user-profile (lambda (user) (column ...))))
(navigate 'cart-screen)         ;; invoke client function directly
(show-dialog :title "New order" :body "...")
```

The server can send both resource deliveries (`put`) and direct client function invocations in the same channel.

---

## The `post` Function: Remote Evaluation

The `post` function enables **remote evaluation** — sending Glue expressions to the server for execution without requiring wrapper functions on the client. This keeps the client minimal and closed.

### How It Works

```clojure
(def amount 1)
(post ('inc 'counter amount))
```

**Evaluation on client:**
1. `'inc` → quoted symbol (not evaluated, preserved as data)
2. `'counter` → quoted symbol (data)
3. `amount` → evaluated to `1`
4. `post` receives the list `(inc counter 1)` as a **payload**
5. `post` serializes and sends `(inc counter 1)` to the server over WebSocket
6. Server receives and evaluates `(inc counter 1)` in its domain environment

The client **never evaluates** the expression — it just forwards it as data.

### Minimal Client Scope

With `post`, the client requires only a handful of closed functions:

| Function | Purpose |
|----------|---------|
| `get` | Request a resource by ID |
| `post` | Send an expression to server for remote evaluation |
| `put` | Update local state (server-initiated) |
| `navigate` | Navigate to a screen |
| `show-dialog` | Show platform dialog |
| `listen` | Subscribe to state changes |
| `lookup` | Get StateNotifier for resource |

All server-side domain logic is invoked via `post` — no wrappers needed.

### `post` vs `get`

| | `get` | `post` |
|---|---|---|
| Purpose | Request a resource by ID | Send an arbitrary expression for evaluation |
| Server response | `put` with resource body | Evaluates expression, may return `put` or result |
| Use case | Fetch state, screens, fragments | Invoke actions, run domain logic |

### Security Considerations

Since `post` can send arbitrary Glue expressions to the server:
- The server should validate and authorize incoming expressions
- Consider implementing a whitelist of allowed operations
- The server environment controls what functions are available

---

## IR Serialization

The Glue IR (Intermediate Representation) contains both serializable data types and non-serializable runtime objects. Understanding this distinction is critical for `post` and any wire communication.

### IR Data Types

```haskell
data IR m
    = Integer Int                    -- serializable
    | Float Double                    -- serializable
    | String Text                     -- serializable
    | Bool Bool                       -- serializable
    | Symbol Text                     -- serializable
    | DottedSymbol [Text]             -- serializable
    | List [IR m]                     -- serializable
    | Object (Map Text (IR m))        -- serializable
    | Void                            -- serializable
    | Evaluable (m (IR m))           -- NOT serializable
    | NativeValue (Value m)           -- NOT serializable
    | NativeFunc (IR m -> m (IR m))  -- NOT serializable
    | Special ([IR m] -> m (IR m))   -- NOT serializable
    | Closure [Text] (IR m) (Env m)   -- NOT serializable
```

### Serializable Types

These types can be converted to AST and sent over the wire:

| Type | Description |
|------|-------------|
| `Integer` | Integer numbers |
| `Float` | Floating point numbers |
| `String` | Text strings |
| `Bool` | Boolean values |
| `Symbol` | Single identifiers |
| `DottedSymbol` | Namespaced identifiers |
| `List` | Lists of IR values |
| `Object` | Key-value maps |

### Non-Serializable Types

These types cannot be transmitted over the wire:

| Type | Description |
|------|-------------|
| `Evaluable` | Monadic thunks (deferred computations) |
| `NativeValue` | Host language objects (Flutter widgets, etc.) |
| `NativeFunc` | Curried host functions |
| `Special` | Special forms (quote, if, define, etc.) |
| `Closure` | Functions with captured environments |
| `Void` | Unit/null type |

### The Quote Solution

The **quote** mechanism (`'`) prevents evaluation and preserves expressions as serializable data:

```clojure
;; Without quote - evaluated on client
(inc counter amount)  ;; returns Void or error if not defined

;; With quote - preserved as data
('inc 'counter amount)  ;; returns List [Symbol "inc", Symbol "counter", ...]
```

**How it works:**
1. Quote stops evaluation at that point
2. The result is raw IR data (symbols, lists, primitives)
3. All serializable → can be sent via `post`
4. Server evaluates the expression in its environment

### Serialization Failures

When serialization fails (trying to send non-serializable IR):

- **Runtime error** - the `post` call fails with a serialization error
- **Partial data** - only serializable parts are sent, causing unexpected behavior
- **Silent failure** - the expression is not sent, leaving the client in an inconsistent state

**Best practices:**
- Always use quote (`'`) when sending expressions via `post`
- Never try to serialize closures, functions, or native values
- The server environment controls what functions are available - client doesn't need native functions

---

## The Resource Lifecycle

### 1. Request
The client needs a resource by ID. Checks in-memory store first.

- **Cache hit** → `StateNotifier` already holds the value; UI is live immediately
- **Cache miss** → client sends `(get 'id)` to the server over WebSocket

### 2. Fetch & Execute (Server)
The server receives the `get` expression, evaluates it in its domain environment, and produces a Glue result — a state value, a screen expression, or a mix.

### 3. Deliver
The server sends `(put 'id body)` back to the client. `body` is the complete Glue source for the resource.

### 4. Cache & Render (Client)
1. Client **persists** the Glue source text locally (keyed by ID) — survives restarts
2. Client **evaluates** the Glue in its Flutter environment
3. Result stored in the **reactive state store** under the resource ID
4. `StateNotifier` fires → all `listen` subscriptions re-render automatically

### 5. Interact
User interactions invoke Glue action expressions — some run locally (client environment), some dispatch to the server. Server may respond with updated `put` messages **and** direct client invocations (e.g., `(navigate 'order-details :id "order-123")` after a successful submit).

---

## Server-Initiated Client Calls

The server is not limited to responding to requests. At any time it can push Glue expressions that invoke registered client functions:

```clojure
(navigate 'order-details :id "order-123")
(show-dialog :title "New message" :body "...")
(put 'cart (:items () :total 0))    ;; clear cart state
```

The client's Glue Controller evaluates these in the client environment — navigation happens, dialogs appear, state updates fire. This enables **full server-driven application flow**, not just server-driven content.

---

## The Two Controllers

|  | Client Controller | Server Controller |
|---|---|---|
| **Input** | Glue text from server via WebSocket | Glue text from client via WebSocket |
| **What it does** | Parse → persist → eval in Flutter env → update state store | Parse → eval in server domain env → serialize result |
| **Output** | Live widgets via `StateNotifier` + `listen` | Glue text (`put` / function calls) |
| **Environment** | Flutter UI modules, `listen`, `read`, `lookup`, navigation, state store | Domain modules: repositories, services, business logic |

---

## Separation of Concerns

| Layer | Responsibility |
|---|---|
| **Server** | All domain knowledge, all UI structure, all business rules, application flow — in Glue |
| **Client** | Glue runtime, Flutter rendering, reactive store, local capabilities — fully generic |
| **Glue** | Shared language, wire protocol, and RPC mechanism |

---

## Startup Behaviour

On cold start, before the WebSocket connects:

1. Load all persisted Glue sources from local persistent storage
2. Compile and evaluate each in the Flutter environment
3. Populate the reactive state store
4. `listen` subscriptions activate — UI renders immediately from cache (**no loading flash**)

Once connected, the server pushes fresh resources and can initiate application flow remotely.

---

## Why Glue as the Protocol?

- **No custom message schemas** — both sides speak the same language
- **Executable behaviour travels over the wire** — not just data, but reactive expressions, lambdas, and composition
- **Symmetric** — same mechanism for both directions, `get` and `put`
- **Extensible** — new capabilities require only registering new functions in the respective environment
- **The client is fully generic** — zero application-specific code on the client

---

## Summary

This is **SDUI taken to its logical extreme** with **bidirectional Glue RPC**. The store holds both state (data) and screens (reactive Glue expressions using `listen`). Screens use `lookup` to bind `StateNotifier`s for any resource ID — triggering lazy fetches from the server when needed — and `listen` to subscribe reactively. Each `listen` is an independent re-render boundary. The client is a runtime, not an application.

---

## Use Case: Multiple Stores

The client can maintain more than one store. Stores differ along two axes:

| | In-Memory only | Persistent (survives restart) |
|---|---|---|
| **Local** | ephemeral local state | local state |
| **Remote** | volatile server-backed | persisted cached server-backed |

Stores are **first-class Glue values**. `lookup` and `put` take the store as their first argument, making the storage target explicit and composable:

```clojure
;; Stores defined once at app or screen initialisation
(def cache create-remote-cache)   ;; server-backed, persisted to disk
(def session create-remote-store) ;; server-backed, in-memory only
(def local create-local-store)    ;; local in-memory only, never fetched
(def secure create-secure-store)  ;; local storage with OS Secure Storage as backend

;; Binding notifiers — each lookup targets a specific store
(def cart         (lookup remote  'cart))
(def current-user (lookup session 'current-user))
(def form-state   (lookup local   'draft-form))

;; Screens use the notifiers as usual
(listen cart
  (lambda (cart)
    (cart-widget cart)))
```

### Who defines the store for `put`?

- **Server-initiated puts** — the server declares the target store explicitly:
  ```clojure
  (put remote  'cart (:items () :total 0))    ;; persistent remote
  (put session 'current-user (:id "u1" ...))  ;; volatile session
  ```
- **Local puts** — the client writes to local stores independently, no server involved:
  ```clojure
  (put local 'draft-form (:name "" :email ""))
  ```

### Who defines the store for `get`?

The client sends `(get 'id)` without specifying a store — it just requests the resource by ID. The **server decides** which store to write to when it responds with `put`. This keeps the client free of policy decisions about remote resource persistence. The client only specifies stores for resources it manages locally.

### Typical store configuration

| Store | Persistence | Remote | Typical use |
|---|---|---|---|
| `remote` | persistent | yes | UI resources, domain state — survives restart, refreshed from server |
| `session` | memory | yes | volatile server state — lost on restart, re-fetched on reconnect |
| `local` | memory | no | form state, selection state, transient UI — client-only |
| `prefs` | persistent | no | user preferences — saved locally, never sent to server |

---

## Multi-Server Architecture

In distributed systems, a single application may span multiple servers (e.g., auth-service, billing-service, core-service). The client connects to multiple servers simultaneously, each providing different resources and domain logic.

### API Changes

The `get` and `post` functions accept a **server-id** as the first parameter to route requests to the appropriate server:

```clojure
;; Single server (default)
(get 'resource-id)
(post ('inc 'counter 1))

;; Multi-server - server-id as first parameter
(get 'auth-server 'user-profile)
(get 'billing-server 'cart)
(get 'core-server 'products)

(post 'auth-server ('refresh-token))
(post 'billing-server ('create-order cart))
```

### Syntax Requirements

Following Glue grammar rules (no mixed content), server-id must be passed as a quoted symbol:

```clojure
;; get: (get server-id resource-id)
(get 'auth-server 'user-profile)
(get 'billing-server 'order-history)

;; post: (post server-id expression)
(post 'auth-server ('logout))
(post 'billing-server ('process-payment (:amount 100)))
```

### Server Registry

The client maintains a registry of available servers:

```clojure
;; Server configuration (typically from app initialization)
(def servers
  (:auth-server   (:url "wss://auth.example.com/glue"))
   :billing-server (:url "wss://billing.example.com/glue"))
   :core-server   (:url "wss://core.example.com/glue"))
```

### Resource Binding

Resources can be bound to specific servers at definition time:

```clojure
;; Bind resources to specific servers
(def user-profile  (get 'auth-server 'user-profile))
(def order-history (get 'billing-server 'order-history))
(def product-list  (get 'core-server 'products))
```

### Cross-Server Considerations

When resources span multiple servers:

1. **Resource References** - A screen may reference resources from different servers. The client handles routing automatically based on server-id.

2. **Consistency** - Each server manages its own state. Cross-server transactions are not atomic - use eventual consistency patterns.

3. **Connection Management** - Each server maintains its own WebSocket connection. Connections are independent - one server going offline doesn't affect others.

4. **Server Discovery** - Server URLs can be discovered at runtime or configured statically.

### Cross-Server Security (XSS-like Issues)

When a screen references resources from multiple servers, there are security implications similar to web XSS:

1. **Server Identity** - Each server must verify the identity of other servers it communicates with. Malicious servers could inject harmful code.

2. **Resource Isolation** - A compromised server should not be able to:
   - Access resources on other servers
   - Execute arbitrary actions on the client
   - Manipulate state meant for other servers

3. **Message Routing** - The client must validate that:
   - Each `put` message comes from the server it claims
   - Resource IDs are scoped to their originating server
   - Server-initiated calls are authorized

4. **Defense Strategies**:
   - **Server certificates** - Mutual TLS between client and servers
   - **Message signing** - Servers sign their messages cryptographically
   - **Resource scoping** - Prefix resources with server-id to prevent collision
   - **Capability tokens** - Servers prove they have permission to act on resources

```clojure
;; Resource IDs scoped by server
(get 'auth-server 'user-profile)      ;; user-profile from auth-server
(get 'billing-server 'user-profile)   ;; user-profile from billing-server

;; Server-signed put
(put 'auth-server 'user-profile (:name "Alice") :signature "...")
```

5. **Trusted Servers List** - The client maintains a list of trusted server identities. Untrusted servers cannot:
   - Send `put` messages for resources they don't own
   - Invoke client functions
   - Access cached resources

### Alternative: Server Context

An alternative approach is to embed the server in the evaluation context rather than passing server-id explicitly:

```clojure
;; No server-id needed - context provides it
(get 'user-profile)    ;; → uses current server context
(post ('refresh-token))  ;; → uses current server context
```

**How it works:**
1. Client maintains evaluation context with current server
2. When evaluating `put` from auth-server, context = auth-server
3. All `get`/`post` calls automatically use that server
4. Context switches when evaluating code from different server

**Advantages:**
- **Simpler API** - no server-id parameter needed
- **Security by default** - client can't be tricked into posting to wrong server
- **Implicit isolation** - each server's code runs in its context naturally

**Security:**
- Server cannot inject cross-server `post` calls - client always uses current context
- Even if server sends malicious data like `(post 'other-server 'action)`, client ignores the explicit server and uses its own context

```clojure
;; Server sends this malicious payload
(:action (post 'billing-server 'steal-money))

;; Client evaluates in auth-server context
(post 'billing-server 'steal-money)  ;; IGNORED!
(post ('steal-money))  ;; → sent to auth-server (correct)
```
