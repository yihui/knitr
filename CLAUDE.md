# Repository Instructions

## Build and Test

```bash
# Build
R CMD build .

# Install
R CMD INSTALL knitr_*.tar.gz

# Run tests
Rscript tests/run-all.R

# Roxygenize (after changing roxygen docs)
Rscript -e 'roxygen2::roxygenize(".")'
```

Tests live in `tests/testit/test-*.R`. Each uses `testit::assert()`:

```r
library(testit)
assert("expectation message", {
  (result %==% expected)
})
```

- Use `has_error()` not `tryCatch()` for error testing.
- Never use `:::` to access internals; testit exposes them automatically.

## R Code Style

1. **Assignment**: `=` not `<-`
2. **Strings**: single quotes in source (e.g. `'text'`), double quotes in `@examples`
3. **Indentation**: 2 spaces
4. **Compact**: no `{}` for single-expression `if`; no `else NULL`; no `return(NULL)`
5. **Roxygen**: no `@description`, `@details`, or `@title` tags
6. **Examples**: double quotes, avoid `\dontrun{}`; spaces around `=`, `+`, `-`
7. **Line length**: wrap at 80 chars

## PR Checklist

1. Add/update tests for every code change
2. Run `roxygen2::roxygenize('.')` after changing roxygen docs; commit updated `man/` files
3. Activate the pre-commit hook once per clone: `git config core.hooksPath .githooks`
4. Run tests before pushing: `Rscript tests/run-all.R`
5. Update `NEWS.md`; first heading is always the dev version: `# CHANGES IN knitr VERSION x.y`
6. Never commit generated artifacts (`.tar.gz`, `.Rcheck/`)
