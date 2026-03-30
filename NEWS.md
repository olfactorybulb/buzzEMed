# buzzMed 0.1.2
* **Function Renaming**: Updated core fitting functions for better clarity on data types:
  * `buzzEBMcat`   -> `buzzEBMcatYcont`
  * `buzzMYcat`  -> `buzzEBMcatYcat`
  * `buzzMYcont` -> `buzzEBMcontYcont`
  * `buzzYcat`   -> `buzzEBMcontYcat`
* **Custom Prior Support**: Users can now manually define prior distributions.
  * Added `make_parms_main()` as a internal central router for parameter handling.
  * Added `make_parms_from_argument()` and `make_parms_from_df()` for flexible input methods.
* **Interactive Features**:
  * Added `run_parms_wizard()`, a CLI tool to guide users through creating prior dataframes interactively.
* **Internal Improvements**:
  * Introduced `utils.R` to house smaller internal helper functions.
---

# buzzMed 0.1.1
* Added unit tests using `testthat` to ensure model stability.
* Added executable examples to all main functions.
* Fixed `.Rbuildignore` to exclude internal development files.

---
# buzzMed 0.1.0
* Initial CRAN submission.
