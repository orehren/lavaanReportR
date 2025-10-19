# Agent Instructions for lavaanReportR

## Core Principle: In-Container Execution

**You are operating inside a pre-built Docker container defined by the `Dockerfile` in this repository's root.**

The environment is already configured with R, Rscript, and all R packages specified in the `renv.lock` file. You MUST NOT attempt to build the environment yourself. Your task is to work directly within this provided environment.

### Workflow

Execute the following commands directly in the shell to perform your tasks.

**1. To regenerate documentation:**

```bash
Rscript -e 'devtools::document()'
```

**2. To run tests:**
```bash
Rscript -e 'devtools::test()'
```

**3. To perform a full package check:**
```bash
Rscript -e 'devtools::check()'
```
