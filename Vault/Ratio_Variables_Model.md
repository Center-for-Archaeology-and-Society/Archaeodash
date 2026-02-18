# Ratio Variables Model

## Summary
Users define numerator/denominator specs; ratios are computed before transformation and can be appended or used exclusively.

## Components
- Spec normalization/validation: `ratio_specs_tbl()`, `build_valid_ratio_specs()`
- Column computation: `add_ratio_columns()`
- Mode resolution: `resolve_final_chem()`
- UI builder modal in `R/datainputTab.R`

## Related
- [[Transformation_Pipeline]]
