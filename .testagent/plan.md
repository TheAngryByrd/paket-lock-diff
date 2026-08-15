# Playwright Test Implementation Plan

## Phase 1: Harness

1. Add exactly `Microsoft.Playwright` 1.62.0 to Paket and reference it from a new net10.0 F# Expecto executable.
2. Register `Browser.Tests.fsproj` in `Application.sln` and `paket-lock-diff.sln`.
3. Add an install mode that delegates to the official Playwright CLI API.
4. Add a local server fixture that starts `deploy/Server.dll` on an ephemeral port, waits for readiness, captures diagnostics, and always terminates the process.
5. Add isolated Chromium contexts and deterministic network fulfillment helpers.

## Phase 2: Original User Flows

| Planned test | Requirement coverage and assertions |
| --- | --- |
| `landing page loads the HTMX and Fable application shell` | Page title, default mode, Fable ready marker, htmx global, navbar destinations, version footer |
| `URL comparison fetches both locks and restores a shared report after reload` | Both remote bodies requested, exact upgrade rendered, query parameters persisted, reload automatically recreates report |
| `GitHub pull request comparison discovers and compares the changed root lock file` | Contents and pull-files APIs called, nested lock ignored, discovered older/newer bodies fetched, exact upgrade rendered, GitHub URL and discovered URLs persisted |
| `raw text comparison renders rich Markdown and JSON reports and preserves the selected output` | Real `/compare` request, exact rich count/version/link, Markdown headings/content, parsed JSON fields, output selection survives a later HTMX swap |
| `input modes preserve in-progress values through tab and history navigation` | URL/raw/GitHub values retained while panels switch; back navigation restores mode without discarding values |
| `copy buttons copy the exact Markdown and JSON reports` | Clipboard permissions granted; clipboard text exactly equals each rendered output |
| `clipboard failure keeps the current comparison and selected output` | Rejected clipboard promise renders a focused error without clearing the report or changing output mode |
| `URL fetch failure reports the HTTP status without submitting invalid content` | Client error names failed URL/status and no comparison output is produced |
| `invalid raw lock text shows a safe comparison error` | Server error is visible, offending parse context is encoded, stack/source paths are absent |
| `a newer raw comparison wins over a delayed URL fetch` | Delayed successful remote work cannot overwrite a newer raw result |
| `a delayed URL error cannot replace a newer raw comparison` | Delayed failed remote work cannot clear a newer raw result or render a stale alert |

## Phase 3: Build Integration and Documentation

1. Add `BuildBrowserTests`, `InstallBrowserTests`, `RunBrowserTests`, and `RunBrowserTestsHeaded` FAKE targets.
2. Preserve the fast `RunTestsHeadless` target and add `RunAllTestsHeadless` as the explicit unit-plus-browser aggregate.
3. Document browser prerequisites and commands in `README.md`.

## Phase 4: Verification and Review

1. Restore/update Paket and normalize the generated restore target's line endings.
2. Build and run the browser project directly, fixing compile/runtime failures without weakening assertions.
3. Run the complete `RunTestsHeadless` target and production bundle.
4. Run Fantomas and a full non-incremental solution build.
5. Review every generated assertion against the production behavior.
6. Run the required assertion-quality and test-gap reviews; record results in `.testagent/status.md`.
