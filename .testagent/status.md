# Playwright Test Status

## Result

- Branch: `codex/giraffe-htmx-migration`
- Migration checkpoint: `857b153 Migrate UI to Giraffe.Htmx`
- Browser project: `tests/Browser/Browser.Tests.fsproj`
- `dotnet run -- RunAllTestsHeadless`: passed
- Full solution build: 0 warnings, 0 errors
- Paket outdated check: no outdated packages; Playwright pinned to 1.62.0
- NuGet vulnerability scan: no vulnerable packages in any solution project
- npm audit: 0 vulnerabilities

## Verification

```text
dotnet run -- RunAllTestsHeadless
Passed

dotnet build Application.sln --no-incremental --no-restore
Build succeeded, 0 warnings, 0 errors
```

The browser harness compiles the Fable client, publishes the server in Release
mode, installs the Playwright-managed Chromium revision, launches the published
application on an ephemeral loopback port, and records diagnostics beneath
`.artifacts/e2e/results` on failure. All non-application HTTP traffic is mocked
through Playwright routes.

## Independent Reviews

The assertion-quality audit found no assertion-free, trivial-only,
self-referential, or remaining P1 false-positive tests. The suite waits for the
prior HTMX fragment to detach, verifies exact query parameters and request
counts, requires copy oracles to contain report-specific content, and checks
active/hidden tab state.

The initial bounded mutation audit exercised nine valid Fable client mutations:
three were killed, four survived, and two exposed uncovered error paths. It
found no P0 gaps. The three P1 gaps were converted into regression coverage for
stale error suppression, exact root-level GitHub lock selection, and clipboard
failure persistence.

A focused recheck applied those three mutations independently to a fresh
temporary copy, forced a no-cache Fable rebuild, and republished the server.
Each mutation was killed by its corresponding regression test, and the fully
restored baseline passed.

## Production Finding

The legacy UI preserved the selected Rich, Markdown, or JSON output across
later comparisons. The migrated fragment initially reset to Rich. The Fable
enhancement now stores the selected output on the document root and reapplies it
after each HTMX swap; the browser suite verifies the behavior after a fresh
server comparison.
