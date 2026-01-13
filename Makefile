# Glue Language Project Makefile
# Provides convenient commands to work with the project from the root directory

.PHONY: help test test-glue analyze analyze-glue format format-glue clean clean-glue pub-get pub-get-glue all

# Default target
help:
	@echo "Glue Language Project Commands:"
	@echo ""
	@echo "Testing:"
	@echo "  make test          - Run all tests"
	@echo "  make test-glue     - Run Glue package tests"
	@echo ""
	@echo "Analysis:"
	@echo "  make analyze       - Run static analysis on all packages"
	@echo "  make analyze-glue  - Run static analysis on Glue package"
	@echo ""
	@echo "Formatting:"
	@echo "  make format        - Format code in all packages"
	@echo "  make format-glue   - Format code in Glue package"
	@echo ""
	@echo "Dependencies:"
	@echo "  make pub-get       - Get dependencies for all packages"
	@echo "  make pub-get-glue  - Get dependencies for Glue package"
	@echo ""
	@echo "Cleanup:"
	@echo "  make clean         - Clean all build artifacts"
	@echo "  make clean-glue    - Clean Glue package build artifacts"
	@echo ""
	@echo "Combined:"
	@echo "  make all           - Run full CI pipeline (format, analyze, test)"

# Testing
test: test-glue
	@echo "✅ All tests passed!"

test-glue:
	@echo "🧪 Running Glue package tests..."
	@cd dart/glue && dart test

# Analysis
analyze: analyze-glue
	@echo "✅ All analysis passed!"

analyze-glue:
	@echo "🔍 Running static analysis on Glue package..."
	@cd dart/glue && dart analyze

# Formatting
format: format-glue
	@echo "✅ All code formatted!"

format-glue:
	@echo "🎨 Formatting Glue package code..."
	@cd dart/glue && dart format .

# Dependencies
pub-get: pub-get-glue
	@echo "✅ All dependencies installed!"

pub-get-glue:
	@echo "📦 Getting dependencies for Glue package..."
	@cd dart/glue && dart pub get

# Cleanup
clean: clean-glue
	@echo "🧹 All packages cleaned!"

clean-glue:
	@echo "🧹 Cleaning Glue package..."
	@cd dart/glue && rm -rf .dart_tool coverage

# Combined CI pipeline
all: format analyze test
	@echo "🎉 All checks passed! Ready for commit."
