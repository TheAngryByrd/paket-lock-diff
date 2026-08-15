# Paket Lock Diff Tool

## What

This is a tool to analyze two [paket.lock](https://fsprojects.github.io/Paket/lock-file.html) files.  The report generates a few lists:

- Additions made
- Removals made
- Version Upgrades
- Version Downgrades

## Why

When looking at git diffs between two lock files, it can be hard to get an overall picture of what changed, especially if you have many [transitive dependencies](https://fsprojects.github.io/Paket/faq.html#transitive).

## How

1. [Give it a try here](https://paket-lock-diff.azurewebsites.net/)
2. Copy and paste [this lock file](https://raw.githubusercontent.com/TheAngryByrd/MiniScaffold/0.22.0/paket.lock) into `Older LockFile` field.
3. Copy and paste [this lock file](https://raw.githubusercontent.com/TheAngryByrd/MiniScaffold/master/paket.lock) into `Newer LockFile` field.
4. The app should analyze the lock files and give you some results.

---
## Architecture

The application renders its page and comparison results on the server with
[Giraffe](https://giraffe.wiki/),
[Giraffe.Htmx](https://git.bitbadger.solutions/bit-badger/Giraffe.Htmx), and the Giraffe view
engine. [htmx](https://htmx.org/) posts the lock-file form and swaps the returned
HTML fragment into the page.

A small [Fable](https://fable.io/) module remains for work that must happen in
the browser: fetching user-provided URLs without introducing a server-side SSRF
endpoint, resolving GitHub pull-request files, maintaining shareable URL state,
switching output tabs, and copying reports to the clipboard.

## Install prerequisites

To build and run the application you need:

* [.NET 10 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/10.0)

The headless client tests additionally require:

* [Node.js 20.19+ or 22.12+](https://nodejs.org/en/download/)
* [npm 10](https://www.npmjs.com/) or higher

## Starting the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:5000` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

To run all server and client tests once:

```bash
dotnet run -- RunTestsHeadless
```

To run server and client tests in watch mode (you can run this command in a
second terminal):

```bash
dotnet run -- WatchRunTests
```

Finally, there are `Bundle` and `Azure` targets that you can use to package your app and deploy to Azure, respectively:

```bash
dotnet run -- Bundle
dotnet run -- Azure
```

## URL and GitHub inputs

URL fetching stays in the browser, so the remote server must permit the request
through CORS. GitHub pull-request comparisons use unauthenticated GitHub API
requests and are therefore subject to GitHub's public rate limits.

Documentation for the main components is available here:

* [Giraffe](https://giraffe.wiki/)
* [Giraffe.Htmx](https://git.bitbadger.solutions/bit-badger/Giraffe.Htmx)
* [htmx](https://htmx.org/docs/)
* [Fable](https://fable.io/docs/)
* [Paket](https://fsprojects.github.io/Paket/)
