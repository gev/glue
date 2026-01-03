# Specification Development Guidelines

These rules guide the creation and maintenance of the Glue language specification:

## Content Adaptation Rules

1. **Host-Language Agnostic**: All content will be written without reference to specific implementation languages (no Haskell, Dart, or TypeScript specifics except where FFI examples are needed).

2. **Minimal Code Examples**: Only include Haskell data type definitions when absolutely essential for clarity. Use pseudocode or plain English descriptions for algorithms and processes.

3. **Source Material**: Extract and adapt content from the existing `glue/*.md` documentation files, synthesizing information from multiple sources to create coherent specification chapters.

4. **Structure Compliance**: Follow the exact folder and file structure created, with each folder containing a README.md introduction.

## Writing Guidelines

5. **Specification Style**: Write in clear, technical language suitable for language implementers. Focus on "what" and "how" rather than "why" (which goes in language-aims.md).

6. **Syntax Examples**: Use Glue code examples liberally, formatted with proper syntax highlighting.

7. **Cross-References**: Include links between related documents using relative paths.

8. **Completeness**: Cover all essential aspects without unnecessary implementation details.

## Content Organization Rules

9. **Chapter Splitting**: Long evaluation and standard library chapters are split into logical sub-documents grouped in folders.

10. **README Files**: Every folder gets an introductory README that outlines the contents and provides navigation.

11. **Consistent Terminology**: Use consistent technical terms throughout (e.g., "property object" not "dict", "atom" for primitive values).

## Quality Assurance Rules

12. **Accuracy**: Ensure all technical details match the existing Glue implementation documentation.

13. **Clarity**: Prefer simple explanations over complex ones, with examples to illustrate concepts.

14. **Completeness**: Each document should be self-contained with necessary context, while referencing related documents for deeper details.

This approach will create a comprehensive, implementable specification that any developer can use to build a Glue interpreter in their chosen host language.
