# Glue Documentation Hub

This directory contains local documentation for external packages and tools used in Glue development, extracted for offline access.

## Available Documentation

### 📦 Package Documentation

#### [code_forge/](code_forge/README.md)
Complete documentation for the `code_forge` package (v5.2.0):
- API reference and usage examples
- Integration guide for Glue development
- Constructor signatures and parameters
- Performance features and limitations
- Maintenance and update instructions

**Key Classes:**
- `CodeForge` - Main code editor widget
- `CodeForgeController` - Text and selection management
- Language support (180+ languages including Dart)

**Integration Status:** ✅ Fully integrated in glue_demo with Dart syntax highlighting

## Documentation Maintenance

### Adding New Package Documentation

1. Create a new folder: `context/docs/{package_name}/`
2. Add comprehensive `README.md` with:
   - Package overview and key features
   - API documentation and examples
   - Integration guides for Glue development
   - Maintenance instructions

### Updating Existing Documentation

When packages are updated:
1. Check the new version's documentation
2. Update local docs to match
3. Update version numbers in README
4. Test integration still works

### Documentation Standards

- **Complete API Coverage**: Include all relevant classes, methods, and constructors
- **Glue-Specific Examples**: Show how to use in Glue development context
- **Offline Access**: Ensure all information is available without internet
- **Maintenance Guide**: Include instructions for updating docs

## Prerequisites Integration

The task documents reference these local docs:

```markdown
- [ ] **Read [context/docs/code_forge/README.md](../../docs/code_forge/README.md)** - Local code_forge API documentation and usage examples
```

This ensures developers can access complete documentation without external dependencies.

## Contributing

When adding new external dependencies to Glue:
1. Create local documentation following this structure
2. Update task prerequisites to include the local docs link
3. Ensure the documentation is comprehensive and up-to-date

This maintains Glue's self-contained documentation approach and reduces external dependencies.
