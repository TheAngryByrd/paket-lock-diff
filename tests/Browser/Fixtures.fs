namespace Browser.Tests

module Fixtures =

    [<Literal>]
    let OlderLockFile =
        """STORAGE: NONE
RESTRICTION: || (== netcoreapp3.1) (== netstandard2.0) (== netstandard2.1)
NUGET
  remote: https://api.nuget.org/v3/index.json
    FSharp.Core (4.7.2)
    FsToolkit.ErrorHandling (1.4)
      FSharp.Core (>= 4.3.4)
"""

    [<Literal>]
    let NewerLockFile =
        """STORAGE: NONE
RESTRICTION: || (== netcoreapp3.1) (== netstandard2.0) (== netstandard2.1)
NUGET
  remote: https://api.nuget.org/v3/index.json
    FSharp.Core (4.7.2)
    FsToolkit.ErrorHandling (1.4.3)
      FSharp.Core (>= 4.3.4)
"""

    [<Literal>]
    let AlternativeOlderLockFile =
        """STORAGE: NONE
NUGET
  remote: https://api.nuget.org/v3/index.json
    FSharp.Core (4.7.1)
"""

    [<Literal>]
    let AlternativeNewerLockFile =
        """STORAGE: NONE
NUGET
  remote: https://api.nuget.org/v3/index.json
    FSharp.Core (4.7.2)
"""

    [<Literal>]
    let OlderLockUrl = "https://locks.example/older/paket.lock"

    [<Literal>]
    let NewerLockUrl = "https://locks.example/newer/paket.lock"

    [<Literal>]
    let GitHubPullRequestUrl = "https://github.com/example/project/pull/42"

    [<Literal>]
    let GitHubContentsUrl =
        "https://api.github.com/repos/example/project/contents/paket.lock"

    [<Literal>]
    let GitHubPullFilesUrl =
        "https://api.github.com/repos/example/project/pulls/42/files?per_page=100"

    [<Literal>]
    let GitHubPullContentsUrl =
        "https://api.github.com/repos/example/project/contents/paket.lock?ref=feature"

    [<Literal>]
    let GitHubNonRootContentsUrl =
        "https://api.github.com/repos/example/project/contents/src/paket.lock?ref=feature"

    [<Literal>]
    let ExpectedUpgrade = "FsToolkit.ErrorHandling - 1.4.0 -> 1.4.3"
