# ReactHome Auth Server

A standards-compliant OAuth2 Auth Server implementation in Haskell.

## OAuth2 Specification Compliance

This implementation adheres to the following RFCs:

### Core Specifications

- [RFC 6749: OAuth 2.0 Auth Framework](https://datatracker.ietf.org/doc/html/rfc6749)
  - §1.3 Grant Types
  - §2 Client Registration
  - §3 Protocol Endpoints
  - §4 Obtaining Auth
  - §5 Issuing an Access Token
  - §6 Refreshing an Access Token

### Additional RFCs

- [RFC 6750: Bearer Token Usage](https://datatracker.ietf.org/doc/html/rfc6750)
  - Token transmission and protection
- [RFC 7009: Token Revocation](https://datatracker.ietf.org/doc/html/rfc7009)
  - Token invalidation mechanisms
- [RFC 7636: Proof Key for Code Exchange (PKCE)](https://datatracker.ietf.org/doc/html/rfc7636)
  - Enhanced security for public clients

## Supported Grant Types

Implemented according to RFC 6749:

1. **Auth Code Grant** (§4.1)
   - Recommended for server-side web applications
   - Provides best security for confidential clients

2. **Resource Owner Password Credentials Grant** (§4.3)
   - Direct authentication with username/password
   - Suitable for highly trusted applications

3. **Client Credentials Grant** (§4.4)
   - Machine-to-machine authentication
   - No user interaction required

4. **Refresh Token Grant** (§6)
   - Obtain new access tokens without user re-authentication

## Key Features

- Strict adherence to OAuth2 specifications
- Comprehensive domain modeling
- Type-level safety
- Detailed error handling
- Extensive documentation with RFC references

## Architecture

```bash
src/
└── ReactHome/
    └── Auth/
test/
└── ReactHome/
    └── Auth/
```

## Getting Started

### Prerequisites

- GHC 9.10 or later
- Cabal 3.8 or later

### Building

```bash
# Clone the repository
git clone https://github.com/gev/reacthome-auth.git
cd reacthome-auth

# Build the project
cabal build

# Run tests
cabal test
```

## Security Considerations

- Follows OAuth2 security best practices
- Implements strict validation for all grant types
- Comprehensive error handling
- References security sections in RFCs:
  - RFC 6749 §10 (Security Considerations)
  - RFC 6750 §5 (Security Threats)

## Contributing

1. Review the OAuth2 specifications
2. Understand the domain models
3. Write tests before implementation
4. Ensure compliance with RFCs

## License

This project is licensed under the BSD-3-Clause License.

## Acknowledgments

- IETF OAuth Working Group
- OAuth2 specification authors
- Haskell community
