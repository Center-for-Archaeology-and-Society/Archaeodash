# Help Navigation and Home Help Link Update (2026-02-18)

## Summary
Improved help usability so users can recover the embedded help view and access help quickly from Home.

## Changes
- Added controls in `Info > Help`:
  - "Open Help in a new tab"
  - "Reload Help" to restore the embedded help document if a link navigates away
- Added a prominent help link near the top of `Home` description content.
- Updated help docs so external links (`http`, `https`, `mailto`) open in a new tab, preserving the embedded help page state.

## Files Changed
- `R/infoTab.R`
- `R/homeTab.R`
- `inst/app/www/help.md`
- `inst/app/www/help.html`

## Related
- [[Index]]
- [[Interaction_Log_2026-02-18]]
