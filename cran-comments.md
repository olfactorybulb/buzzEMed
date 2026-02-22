## Resubmission
This is a resubmission of buzzMed 0.1.1. This update fixes WARNINGS generated from incoming automated checks.

## Test environments
* Local macOS (aarch64): R 4.4.2
* Windows Server (via win-builder): R-devel (2026-02-18 r89435 ucrt)

## R CMD check results
0 errors | 0 warnings | 1 note
* The note is regarding the unable to verify current time (local environment artifact).

## Fixes
* Added @examples to exported functions to exercise the code.
* Added a unit testing suite using 'testthat' to verify function outputs.
* Updated .Rbuildignore to exclude the .github directory.
* Added a WORDLIST to handle the 'buzzMed' spelling note.
