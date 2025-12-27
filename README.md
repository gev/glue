# ReactHome

A comprehensive IoT platform built in Haskell, featuring a custom Lisp-inspired scripting language for device automation and control.

## 🏗️ Project Structure

This is a multi-package Haskell project organized as follows:

```
reacthome/
├── assets/                 # Static assets (logos, icons)
├── assist/                 # Voice assistant service
├── auth/                   # OAuth2 authentication server
├── core/                   # Core shared functionality
├── daemon/                 # System daemon
├── jose/                   # JOSE (JSON Web Tokens) utilities
├── lang/                   # Language processing utilities
├── reactor/                # Reactor scripting language
│   ├── src/Reactor/        # Language implementation
│   ├── test/               # Language tests
│   └── README.md           # Language documentation
├── reactor-ext/            # VS Code extension for Reactor
├── relay/                  # Message relay service
├── rest/                   # REST API utilities
├── server/                 # Main server application
├── util/                   # Utility libraries
├── ws/                     # WebSocket implementation
├── etc/                    # System configuration files
│   └── systemd/            # Systemd service files
├── cabal.project           # Cabal multi-package configuration
└── README.md               # This file
```

## 📦 Packages

- **reactor**: A Lisp-inspired scripting language for IoT automation
- **reacthome-auth**: OAuth2-compliant authentication server
- **reacthome-assist**: Voice assistant integration
- **reacthome-daemon**: Background service for device management
- **reacthome-relay**: Message routing and relay service
- **reacthome-server**: Main application server
- **core**: Shared core functionality
- **util**: Utility functions and helpers
- **rest**: REST API client/server utilities
- **ws**: WebSocket communication
- **jose**: JWT and cryptographic utilities
- **lang**: Natural language processing

Built with Haskell for reliability and performance in IoT applications.
