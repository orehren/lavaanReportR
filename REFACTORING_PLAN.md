# Comprehensive Refactoring Plan for lavaanReportR

This document outlines a detailed plan to refactor the `lavaanReportR` package, elevating it to the highest standards of quality, maintainability, and elegance, following modern R best practices. The plan is based on the core principles of the "Never-Nesting" philosophy, code readability, and dependency minimization.

## 1. Overall Architecture and Structure

### Assessment
The current architecture is strong, based on a logical five-phase S3-based pipeline: `analyze`, `configure`, `prepare`, `build`, and `render`. This separation of concerns is excellent and provides a solid foundation for the package.

The file structure in the `R/` directory, however, could be improved. It is currently a flat list of API definitions and helper files, which obscures the connection between the files and the pipeline stages they serve.

### Recommendations
To improve clarity and maintainability, I recommend reorganizing the `R/` directory to mirror the five-phase pipeline:

-   **`R/01-analysis.R`**: To contain all functions related to the `analyze` phase (e.g., from `plot_analysis_helper.R`).
-   **`R/02-configuration.R`**: For functions related to the `configure_plot` phase (e.g., from `plot_config_helper.R`).
-   **`R/03-preparation.R`**: For functions related to the `prepare` phase (e.g., from `plot_prepare_helper.R`).
-   **`R/04-build.R`**: For functions that construct the DOT code in the `build` phase (e.g., from `plot_build_helper.R`).
-   **`R/05-render-api.R`**: To contain the user-facing S3 generics and methods from `plot_S3_api.R`, serving as the package's public interface.
-   **`R/utils.R`**: For genuinely shared utility functions that are used across multiple pipeline stages.
-   **`R/constants.R`**: To centralize all constants currently defined in `plot_S3_constants.R`.

This structure will make the codebase more intuitive to navigate and easier to maintain.

## 2. Dependency Management (`DESCRIPTION` file)

### Assessment
The `DESCRIPTION` file lists several dependencies. A critical review is necessary to ensure the package remains as lightweight as possible.

-   **`psych`**: This package is used in a single location (`.validate_reliability_ellipsis`) to introspect the arguments of the `psych::alpha` function. This is non-core functionality and introduces a heavy dependency.
-   **`purrr`**: The codebase uses several functions from `purrr` (e.g., `compact`, `map`, `detect`), but the package is not listed in `Imports`.
-   **`igraph`**, **`DiagrammeR`**, **`data.table`**, **`lavaan`**: These dependencies are central to the package's core functionality (layout, rendering, data manipulation, and the core SEM object) and are justified.

### Recommendations
1.  **Move `psych` to `Suggests`**: The dependency on `psych` should be made optional.
    -   Modify `.validate_reliability_ellipsis` to first check if the package is installed using `requireNamespace("psych", quietly = TRUE)`.
    -   If `psych` is not available, the function should skip the argument validation and perhaps issue a friendly warning or message, allowing the rest of the package to function normally.
2.  **Add `purrr` to `Imports`**: To ensure the package is self-contained, `purrr` must be formally declared as a dependency.
    -   Add `purrr` to the `Imports` section of the `DESCRIPTION` file.
    -   Use `roxygen2` tags (`@importFrom purrr compact map ...`) to selectively import only the functions that are used.
3.  **Keep Core Dependencies**: No changes are recommended for `igraph`, `DiagrammeR`, `data.table`, and `lavaan`.

## 3. Code Refactoring Opportunities

This section identifies specific violations of the "Never-Nesting" rule (max two levels of indentation) and other code smells.

### File: `R/plot_analysis_helper.R`

-   **Function: `.resolve_to_path_labels`**
    -   **Violation**: A chain of `if` statements creates unnecessary nesting.
    -   **Strategy**: Refactor using guard clauses to handle the base cases (atomic paths) at the beginning of the function. This will flatten the structure, leaving the recursive step in the main, un-nested body of the function.

-   **Function: `.modify_moderated_edges`**
    -   **Violation**: Not a nesting violation, but a code smell. The use of `lapply` to modify the `edges` data.table by reference creates side effects within a functional-style loop, which can be confusing and hard to debug.
    -   **Strategy**: Rewrite using a declarative `data.table` update join. This involves creating a lookup table from the moderation edges and then using it to update the main `edges` table in a single, clear operation. This approach is more idiomatic, efficient, and readable.

### File: `R/validation_helper.R`

-   **Function: `.validate_function_args`**
    -   **Violation**: The `for` loop contains a complex, multi-case `switch` statement, increasing the cognitive load.
    -   **Strategy**: Extract the logic for building the `contextual_args` list into a new, dedicated helper function. The main loop will then call this function, reducing nesting and simplifying the body of the loop.

-   **Function: `.assert_argument_is_plausible_string_selector`**
    -   **Violation**: **Severe (3+ levels)**. A series of `if-else if` statements contains a nested `for` loop, which in turn contains another `if-else if` block.
    -   **Strategy**: This function requires a complete rewrite.
        1.  Break it down into smaller, single-responsibility helper functions (e.g., `.validate_character_vector_selector`, `.validate_call_selector`, `.validate_symbol_selector`).
        2.  Use guard clauses extensively in the main function and the new helpers to handle different cases (NULL, character, call, symbol) without nesting.

## 4. Documentation and Testing

### Assessment
-   **Documentation**: The `roxygen2` documentation is present for the main API but is incomplete for many internal helper functions. Some internal functions may be inadvertently exported.
-   **Testing**: The package currently has **0% test coverage**, as no `testthat` infrastructure or tests exist. This is a critical gap.

### Proposed Strategy

#### Documentation
1.  **Audit All Functions**: Review every function in the package.
2.  **Enforce `@noRd`**: Ensure all internal helper functions are tagged with `@noRd` to prevent them from being exported in the package namespace or documentation.
3.  **Complete Docstrings**: For all exported functions, ensure every parameter (`@param`) and the return value (`@return`) is thoroughly documented.
4.  **Add Examples**: Provide runnable examples (`@examples`) for all primary user-facing functions to demonstrate typical usage.

#### Testing
1.  **Initialize `testthat`**: Set up the testing infrastructure using `usethis::use_testthat()`.
2.  **Adopt a Structured Approach**: Create test files that mirror the refactored file structure (e.g., `test-01-analysis.R`, `test-02-configuration.R`).
3.  **Develop Unit Tests**:
    -   **Analysis**: Create mock `lavaan` parameter tables to test that nodes, edges, and features are correctly identified for various model types (Path, CFA, LGM, multigroup).
    -   **Configuration**: Test that user arguments correctly override defaults and that plot recipes are assembled as expected.
    -   **Validation**: Write dedicated tests for the validation helpers to ensure they correctly catch invalid user input.
4.  **Implement Snapshot Testing**:
    -   **Build Phase**: For the `build` phase, use `testthat::expect_snapshot_output()` to test that the generated DOT code is correct and remains consistent for a variety of models. This is an efficient way to detect regressions in the plot structure.
