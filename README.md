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
## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* [.NET SDK](https://www.microsoft.com/net/download) 8.0 or higher
* [Node 18](https://nodejs.org/en/download/) or higher
* [NPM 9](https://www.npmjs.com/package/npm) or higher

## Starting the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

To run concurrently server and client tests in watch mode (you can run this command in parallel to the previous one in new terminal):

```bash
dotnet run -- WatchRunTests
```

Client tests are available under `http://localhost:8081` in your browser and server tests are running in watch mode in console.

Finally, there are `Bundle` and `Azure` targets that you can use to package your app and deploy to Azure, respectively:

```bash
dotnet run -- Bundle
dotnet run -- Azure
```

## SAFE Stack Documentation

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
