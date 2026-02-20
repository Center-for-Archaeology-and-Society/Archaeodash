# Upload Append Type Harmonization Fix 2026-02-20

Issue: Appending newly uploaded rows to an existing workspace could fail in `bind_rows()` when matching columns had incompatible types (for example numeric in existing data vs character in incoming data).

## Change
- Updated `merge_loaded_data()` in `R/DataLoader.R` to harmonize shared column types before `bind_rows()`.
- For each shared column with mismatched storage types:
  - If both sides are numeric-like, coerce both to numeric.
  - Otherwise coerce both to character.
- Added regression tests in `tests/testthat/test-dataLoader.R` covering both numeric-like coercion and non-numeric fallback behavior.

## Outcome
- Upload append mode no longer errors on numeric/character mismatch for shared columns.
- Numeric columns remain numeric when feasible; mixed semantic columns safely degrade to character.
