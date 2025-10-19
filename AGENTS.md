# Agent Instructions for lavaanReportR

<<<<<<< HEAD
## Core Workflow: Docker-Based Development

This project's environment is defined by the `Dockerfile`. All development, testing, and verification commands MUST be run inside a container built from this file to ensure perfect reproducibility.

### 1. Build the Docker Image

Before starting any work, build the Docker image from the Dockerfile. This image will contain all necessary dependencies.

```bash
docker build -t lavaan-dev
```

2. Run Commands Inside the Container
All subsequent commands must be executed within the lavaan-dev container. Use the following docker run format, which mounts the current project directory into the container's /work directory.

Replace "<command-to-run>" with the specific command you need to execute.

```bash
docker run --rm -v "$(pwd)":/work lavaan-dev <command-to-run>
```

Required Verification Commands
After making any code changes, you MUST run the following verification commands using the format described above.

Regenerate Documentation and Namespace:

```bash
docker run --rm -v "$(pwd)":/work lavaan-dev Rscript -e 'devtools::document()'
```

Run the Test Suite:

```bash
docker run --rm -v "$(pwd)":/work lavaan-dev Rscript -e 'devtools::test()'
```

Perform a Full Package Check:

```bash
docker run --rm -v "$(pwd)":/work lavaan-dev Rscript -e 'devtools::check()'
=======
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
>>>>>>> feature/refactoring-plan
```
