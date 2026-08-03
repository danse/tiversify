
The business logic is quite essential and easy to follow, it's at
`app/Business.hs`. X-specific calls (OAuth 1.0a signing and X API v2
endpoints) go to `app/X.hs`. The executable entry point is
`app/Main.hs`.

## What's implemented

`app/X.hs` — OAuth 1.0a user-context signing (hand-rolled HMAC-SHA1
via `crypton` + `base64-bytestring`) plus three X API v2 calls, all
using `http-client`:

- `followed` (`app/X.hs:234`) — followed accounts, with pagination
- `dailyOutput` (`app/X.hs:251`) — tweet count in the last 24h via
  `start_time`, paginated (capped at 10 pages)
- `lookupUserId` — resolves handles to numeric IDs
- Credentials read from `X_API_KEY`, `X_API_SECRET`, `X_ACCESS_TOKEN`,
  `X_ACCESS_SECRET`

`app/Business.hs` — pure (no IO, no undefined bindings). Fixes to the
original blueprint that kept it from compiling: `newtype` with two
fields → `data`, `NoFieldSelectors` breaking `.percent`, missing `Ord`
for `Percent`, and the `percent`-returns-`Either` type error in
`computeRates`.

`app/Main.hs` — parses `<handle>`, runs `followed`/`dailyOutput`,
prints the report, exits 1 on API/auth errors.

## Validation

HMAC matches Python exactly, and all signatures match an independent
Python reference. Note: RFC 5849's own example is internally
inconsistent, so validation was done on realistic inputs instead.

Not verified: live calls against the X API (needs real credentials
exported from a developer-portal app with OAuth 1.0a user context).

## Known gaps / next steps

- `dailyOutput` re-resolves the user ID for each followed account; could
  be optimized by threading IDs through
