# Agent Instructions for the lavaanReportR Repository

## Core Requirement: Development Environment

**All development, testing, package building, and command execution MUST occur within the Docker environment defined by the `Dockerfile` in this repository's root.**

This is a non-negotiable rule to ensure a consistent and reproducible workflow. The agent must assume that the host system has no R-related tools installed. All necessary tools (R, devtools, R packages, system libraries) are exclusively provided by the Docker image.

### Guiding Principles

1.  **Environment is Pre-configured:** You will be operating inside an environment already built from the `Dockerfile`. You do not need to run `docker build` or `docker run` commands yourself.
2.  **Tooling:** Rely solely on the tools installed within the environment. For example, use `Rscript`, `devtools::check()`, `devtools::test()`, etc., directly from the command line inside the container.
3.  **Verification:** Before finalizing any task, ensure the package passes a full `devtools::check()` within this environment to confirm it is valid and complete.
