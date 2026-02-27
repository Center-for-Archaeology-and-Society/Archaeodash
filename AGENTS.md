# AGENTS.md

## Vault Checklist (Required)

For every substantive user interaction in this repository:

1. Start from `Vault/Index.md` and follow only relevant links.
2. Capture a brief interaction summary in the current daily interaction log:
   - File pattern: `Vault/Interaction_Log_YYYY-MM-DD.md`
   - One short bullet per interaction (request + outcome).
3. If the daily interaction log does not exist, create it and link it from `Vault/Interaction_Log.md`.
4. If `Vault/Interaction_Log.md` does not exist, create it and link it from `Vault/Index.md`.
5. For non-trivial changes, create an atomic note and link it from `Vault/Index.md` or an appropriate MOC.
6. Before final response, verify there are no orphan vault notes (every new note linked from an index or MOC).

## Definition

- Substantive interaction: any request that changes code, docs, config, process, or project state.

## Security Rule (Vault)

- Never store secrets, credentials, tokens, private keys, PII, or regulated/sensitive data in `Vault/` notes.
