# Playwright Flow Research

## Scope

- User requirement: "Make sure the original user flows still work, create playwright tests (written in F#)"
- Application under test: the Giraffe.Htmx migration committed as `857b153`
- Strategy: broad end-to-end coverage of the original browser-visible workflows
- Browser: Playwright-managed Chromium, headless by default
- Test language and runner: F# with the repository's existing Expecto convention

## Original Flow Inventory

The pre-migration Elmish client exposed these user-visible behaviors:

1. Compare lock files fetched from two arbitrary URLs.
2. Persist the URL pair in the query string and automatically compare shared links on load.
3. Resolve a GitHub pull-request URL through the GitHub contents and pull-files APIs, then compare the root `paket.lock` files.
4. Compare two pasted lock files.
5. Switch among URL, GitHub, and raw-text inputs without losing entered state.
6. View rich, Markdown, and JSON versions of a report.
7. Copy Markdown and JSON output to the clipboard.
8. See useful loading/error states and retain page navigation/version information.

## Current Contracts to Exercise

| Area | Production contract | Browser evidence |
| --- | --- | --- |
| Application boot | `/`, packaged htmx script, `/output/App.js` | title, default URL panel, `data-paket-lock-client-ready` |
| URL input | `#url-compare-form`, `olderLockFileUrl`, `newerLockFileUrl` | fulfilled remote fetches, `/compare` HTMX swap, URL query state |
| Shared URL | `olderLockFileUrl` and `newerLockFileUrl` query parameters | reload auto-fetches and recreates the report |
| GitHub input | `#github-compare-form`, `githubPullRequestUrl` | expected GitHub API calls, discovered download URLs, report output |
| Raw input | `#raw-compare-form`, `#older-lock-text`, `#newer-lock-text` | real HTMX form post and parsed comparison |
| Input state | `[data-input-tab]`, `[data-input-panel]`, `popstate` | values survive mode switches and back navigation |
| Output modes | `[data-output-tab]`, `[data-output-panel]` | exact rich summary plus meaningful Markdown/JSON fields |
| Clipboard | `[data-copy-target]` | actual Chromium clipboard value equals rendered report |
| Errors | `[data-client-fetch-error]`, server comparison alert | HTTP status is useful; malformed lock response has no stack trace |
| Shell | navbar and footer | original destinations and version labels remain visible |

## Determinism and Isolation

- Run the production-style output from `.artifacts/e2e/server/Server.dll` on an ephemeral loopback port.
- Build the Fable client and publish the server before starting the tests.
- Use a fresh Playwright browser context for every test.
- Fulfill arbitrary URL and GitHub API traffic with Playwright routing. Add CORS headers because production fetches occur in the browser.
- Do not contact GitHub, raw-content hosts, CDNs, or the deployed application.
- Use small deterministic `paket.lock` fixtures that make exact browser assertions easy to read.
- Sequence the suite to keep process logs and browser interactions easy to diagnose.

## Project and Commands

- New project: `tests/Browser/Browser.Tests.fsproj`
- Package: `Microsoft.Playwright` 1.62.0, pinned exactly because its browser revision must match
- Scoped build: `dotnet build tests/Browser/Browser.Tests.fsproj --no-incremental`
- Browser install: `dotnet run --project tests/Browser/Browser.Tests.fsproj -- install chromium`
- E2E run: `dotnet run -- RunBrowserTests`
- Complete regression run: `dotnet run -- RunAllTestsHeadless`
- Final solution build: `dotnet build Application.sln --no-incremental --no-restore`

## Acceptance Checklist

- [x] Every original entry point (URL, GitHub pull request, raw text) produces a real comparison.
- [x] URL-backed reports remain shareable and automatically restore on reload.
- [x] Input mode switching and browser back navigation preserve in-progress values.
- [x] Rich, Markdown, and JSON reports expose the expected package/version change.
- [x] Markdown and JSON copy buttons write the exact report text to the clipboard.
- [x] Browser-fetch and server-parse failures produce actionable, safe errors.
- [x] The page shell, local htmx asset, and compiled Fable enhancement load successfully.
- [x] The project is registered in both tracked solutions and the FAKE test harness.
- [x] The browser suite, existing tests, production bundle, and non-incremental solution build pass.
