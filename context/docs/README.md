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

#### Automated Documentation Fetch (Recommended)

```bash
# Fetch latest documentation for a specific package
PACKAGE_NAME="code_forge"
VERSION="latest"

# Download HTML documentation
curl -s "https://pub.dev/documentation/${PACKAGE_NAME}/${VERSION}/" > /tmp/${PACKAGE_NAME}_docs.html

# For API-specific pages, fetch individual class docs
curl -s "https://pub.dev/documentation/${PACKAGE_NAME}/${VERSION}/${PACKAGE_NAME}/CodeForge-class.html" > /tmp/${PACKAGE_NAME}_api.html
```

#### Manual Documentation Extraction

1. **Fetch HTML Documentation:**
   ```bash
   # Main package page
   curl -s "https://pub.dev/packages/code_forge" > code_forge_package.html

   # API documentation
   curl -s "https://pub.dev/documentation/code_forge/latest/" > code_forge_api.html

   # Specific class documentation
   curl -s "https://pub.dev/documentation/code_forge/latest/code_forge/CodeForge-class.html" > codeforge_class.html
   ```

2. **Extract Key Information:**
   ```bash
   # Extract constructor signatures
   grep -A 10 "CodeForge(" codeforge_class.html

   # Extract method documentation
   grep -A 5 "text" codeforge_class.html

   # Extract class descriptions
   grep -A 3 "class CodeForge" codeforge_class.html
   ```

3. **Transform to Markdown:**
   ```bash
   # Convert HTML headers to markdown
   sed 's/<h1/<#/g; s/<\/h1/>/<\/#/g' code_forge_api.html > temp.md
   # (Additional conversion steps would be needed for full HTML->Markdown)
   ```

4. **Update Local Documentation:**
   - Copy extracted information to `context/docs/code_forge/README.md`
   - Update version numbers and dates
   - Add new features or API changes
   - Test examples still work with new version

#### Alternative: Pandoc for HTML->Markdown Conversion

```bash
# Install pandoc (if not available)
brew install pandoc  # macOS
# apt install pandoc  # Linux

# Convert HTML to Markdown
pandoc -f html -t markdown code_forge_api.html > code_forge_api.md

# Clean up and format for documentation
# (Manual editing may be needed for optimal formatting)
```

#### Testing Updated Documentation

After updating:
1. Update version numbers in README headers
2. Test integration examples still compile
3. Verify all API calls match new signatures
4. Update any breaking change notes

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
