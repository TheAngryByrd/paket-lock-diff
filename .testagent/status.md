# Playwright Test Status

## Result

- Branch: `codex/giraffe-htmx-migration`
- Migration checkpoint: `857b153 Migrate UI to Giraffe.Htmx`
- Browser project: `tests/Browser/Browser.Tests.fsproj`
- Browser suite: 11 passed, 0 failed, 0 errored
- Existing Expecto suite: 8 passed
- Existing Fable/Mocha suite: 23 passed
- Full solution build: 0 warnings, 0 errors
- Paket outdated check: no outdated packages; Playwright pinned to 1.62.0
- NuGet vulnerability scan: no vulnerable packages in any solution project
- npm audit: 0 vulnerabilities

## Verification

```text
dotnet run -- RunBrowserTests
11 passed, 0 failed, 0 errored

dotnet run -- RunAllTestsHeadless
8 Expecto + 23 Fable/Mocha + 11 Playwright passed

dotnet build Application.sln --no-incremental --no-restore
Build succeeded, 0 warnings, 0 errors
```

The browser harness compiles the Fable client, publishes the server in Release
mode, installs the Playwright-managed Chromium revision, launches the published
application on an ephemeral loopback port, and records diagnostics beneath
`.artifacts/e2e/results` on failure. All non-application HTTP traffic is mocked
through Playwright routes.

## Independent Reviews

The final assertion-quality audit counted 11 tests, 63 distinct `Expect.*` call
sites, and 134 assertion evaluations on the passing path. It found no
assertion-free, trivial-only, self-referential, or remaining P1 false-positive
tests. The suite waits for the prior HTMX fragment to detach, verifies exact
query parameters and request counts, requires copy oracles to contain
report-specific content, and checks active/hidden tab state.

The initial bounded mutation audit exercised nine valid Fable client mutations:
three were killed, four survived, and two exposed uncovered error paths. It
found no P0 gaps. The three P1 gaps were converted into regression coverage for
stale error suppression, exact root-level GitHub lock selection, and clipboard
failure persistence.

A focused recheck applied those three mutations independently to a fresh
temporary copy, forced a no-cache Fable rebuild, and republished the server.
Each mutation was killed by its corresponding new test (10/11), and the fully
restored baseline passed 11/11.

## Production Finding

The legacy UI preserved the selected Rich, Markdown, or JSON output across
later comparisons. The migrated fragment initially reset to Rich. The Fable
enhancement now stores the selected output on the document root and reapplies it
after each HTMX swap; the browser suite verifies the behavior after a fresh
server comparison.
