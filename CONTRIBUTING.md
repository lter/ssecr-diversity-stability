## SSECR Diversity-Stability Scientific Computing Contributing Guidelines

This document is meant to focus primarily on our _internal_ processes for contributing. If you're not a member of this team, we hope you find this document valuable as-is but you should also feel free to make any modifications you feel are necessary if you choose to use our guidelines as a starting place.

### Version Control & GitHub

Be descriptive and consise in your commit messages. Consider using the style of "[Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)" in your commit messages. See [here](https://njlyon0.github.io/tips/commits.html) for a nice summary of the highlights of that approach.

For independant work/experimentation, work in your "junk" script (junk_firstname). Once changes are ready to be incorporated into the shared scripts, see below: 

For **minor changes**, (1) communicate with the team to avoid merge conflicts and (2) make commits directly to `main`. Most changes, including code/analysis additions will be minor.

For **_major_ changes**, (1) communicate with the team to avoid merge conflicts, (2) open a branch with an intuitive but concise name, and (3) open a pull request when you are confident your edits are largely complete. (4) Request a review from a teammate, (5) make any edits they suggest, (6) merge the PR, and (7) delete the branch.

### Style Guide

For folder/file names and in code, please follow the following style tips:

- Use all lowercase
- Related files/objects should have a shared file prefix (e.g., `wrangle_...`)
    - For files that have an inherent order, use zero-padded numbers as the prefix (e.g., `01_`, `02_`, etc.)
- Use underscores (`_`) for separating major pieces of information and hyphens (`-`) in lieu of spaces (e.g., `01_find-area.R`, `harmonize_spp-rich-info.py`, etc.)

### File Paths

When we write code--for ourselves or for working groups--we use **operating system (OS) agnostic file paths**.

In R, this means using the `here::here()` function (from base R) to stitch together elements (e.g., `here::here("data", "raw_2024.csv")`). In Python it means using the `join` function from the `path` module of the `os` library (e.g., `os.path.join("data", "raw_2024.csv")`).

If you plan on re-using a file path, assign it to an object/variable (R/Python) to avoid typos/errors when re-using the file path.
