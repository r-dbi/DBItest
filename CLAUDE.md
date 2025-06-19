# Claude Development Guidelines for DBItest

## Project Overview

DBItest is a comprehensive testing framework for database interface (DBI) implementations in R.
The package provides standardized compliance tests to ensure DBI implementations work correctly across different database backends.
This document provides guidelines for AI assistants working on this codebase.

## Code Style and Documentation

### Comment Style
- Add comprehensive comments to utility functions that aren't immediately obvious
- Use line breaks after each sentence in multi-sentence comments
- Focus comments on explaining the "why" and "how", not just the "what"
- Include context about the function's role in the testing framework

### R Code Conventions
- Follow existing naming conventions (snake_case for functions, camelCase for some legacy functions)
- Use explicit package prefixes (e.g., `withr::local_db_connection`) for clarity
- Maintain consistent indentation and spacing patterns
- Use meaningful variable names that reflect the testing context

## Testing Framework Architecture

### Core Components
- **Context Management**: Functions in `context.R` handle test environment setup
- **Test Execution**: Functions in `run.R` manage the test lifecycle and filtering
- **Utilities**: Functions in `utils.R` provide connection management and test helpers
- **Expectations**: Functions in `expectations.R` define custom test assertions
- **S4 Introspection**: Functions in `s4.R` analyze S4 method implementations

### Key Principles
- Tests must be isolated and not interfere with each other
- Resources (connections, tables, result sets) must be properly cleaned up
- Error handling should be robust to prevent test framework failures
- Support for different database backends requires flexible configuration

## Development Guidelines

### When Adding Comments
- Focus on complex utility functions that manage resources or state
- Explain the purpose of test helper functions in the context of DBI testing
- Document any non-obvious patterns or workarounds for database compatibility
- Include information about cleanup and resource management

### Code Quality Checks
- Ensure all database connections are properly closed
- Verify that test tables are cleaned up after use
- Check that result sets are cleared to prevent resource leaks
- Validate that error conditions are handled appropriately

### Testing Approach
- Use the existing test infrastructure in `tests/testthat/`
- Follow the pattern of creating isolated test contexts
- Ensure new functionality works across different DBI implementations
- Test both success and failure scenarios

## Maintenance Notes

### Package Dependencies
- Core dependencies: DBI, testthat, withr, methods
- Optional dependencies: Various database drivers for testing
- Keep dependency usage minimal and well-justified

### Backwards Compatibility
- Maintain compatibility with existing DBI implementations
- Use feature detection rather than version checks when possible
- Provide clear migration paths for deprecated functionality

### Performance Considerations
- Test execution should be efficient to avoid long CI times
- Resource cleanup should be automatic to prevent memory leaks
- Connection pooling and reuse where appropriate

## Common Patterns

### Resource Management
```r
# Pattern for automatic cleanup
local_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  withr::local_db_connection(con)
}
```

### Test Context Setup
```r
# Pattern for test context creation
make_context <- function(drv, tweaks = NULL) {
  # Validate inputs
  # Create context structure
  # Set defaults
}
```

### Error Handling
```r
# Pattern for safe cleanup operations
try_silent <- function(code) {
  tryCatch(code, error = function(e) NULL)
}
```

## Future Considerations

- Monitor DBI specification changes and update tests accordingly
- Consider performance improvements for large test suites
- Evaluate new database backends as they become available
- Keep documentation synchronized with code changes