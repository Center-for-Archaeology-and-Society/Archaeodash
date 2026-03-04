# Logout Button Reliability and Remember Token Revocation (2026-03-04)

## Context
- Reported behavior: in beta, clicking `logout` appeared to do nothing.
- Local repro showed logout UI state changed correctly, but remembered-token revocation depended on event timing.

## Root Cause
- Server revocation on logout used `input$remembered_token_revoke` inside the `input$logoutUI` observer.
- `remembered_token_revoke` is sent from client JS only after server sends the `auth_cookie: clear` message, so it can arrive after the logout observer executes.
- This created a race where logout state could clear locally, but server token revocation was not guaranteed.

## Implementation
- Added `revoke_known_remember_tokens(...)` helper in `R/loginServer.R` to safely revoke one or more token candidates.
- Added `observeEvent(input$remembered_token_revoke, ...)` so revocation runs when the client sends token revoke input.
- Updated logout and cookie-decline flows to revoke both known token inputs (`remembered_token_revoke`, `remembered_token`) before clearing client cookie state.
- Added a JS-triggered logout fallback event (`logout_click_js`) on the logout button `onclick`, with matching server/auth observers so logout still executes if the standard `actionButton` path is flaky in a client session.

## Validation
- Unit login-security test passes when run in package context (`devtools::load_all()` then test file).
- Added e2e regression test: `tests/testthat/test-shinytest2-logout-e2e.R`.
- Direct local shinytest2 repro confirms logout toggles UI state (`logout` hidden, `login` shown) and auth cookie cleared.
- DOM-click repro confirmed `logout_click_js` input is emitted and logout state transitions complete.
