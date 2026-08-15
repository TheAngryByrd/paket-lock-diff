module Client.Tests

open Fable.Mocha
open Index

let private expectPullRequestInfo url expected =
    match GitHub.tryParsePullRequestUrl url with
    | Ok actual -> Expect.equal actual expected "The URL should identify the expected pull request"
    | Error error ->
        failtestf "Expected a valid GitHub pull request URL, but parsing failed: %s" error

let private expectInvalidPullRequestUrl url =
    match GitHub.tryParsePullRequestUrl url with
    | Ok actual -> failtestf "Expected %s to be rejected, but it parsed as %A" url actual
    | Error error ->
        Expect.isNotEmpty error $"Invalid URL {url} should have a useful validation error"

let private invalidPullRequestUrls = [
    "rejects an empty value", ""
    "rejects a relative URL", "github.com/owner/repository/pull/1"
    "rejects another host", "https://example.com/owner/repository/pull/1"
    "rejects a deceptive GitHub suffix", "https://github.com.example/owner/repository/pull/1"
    "rejects a non-default port", "https://github.com:444/owner/repository/pull/1"
    "rejects user information", "https://user@github.com/owner/repository/pull/1"
    "rejects a non-HTTP scheme", "ftp://github.com/owner/repository/pull/1"
    "rejects an issue URL", "https://github.com/owner/repository/issues/1"
    "rejects a missing pull request number", "https://github.com/owner/repository/pull"
    "rejects a non-numeric pull request number",
    "https://github.com/owner/repository/pull/not-a-number"
    "rejects pull request number zero", "https://github.com/owner/repository/pull/0"
    "rejects an encoded slash in a path segment",
    "https://github.com/owner%2Frepository/project/pull/1"
    "rejects a Unicode repository owner", "https://github.com/ownér/repository/pull/1"
    "rejects an owner that starts with a hyphen", "https://github.com/-owner/repository/pull/1"
    "rejects a malformed percent escape", "https://github.com/owner/repository/pull/%ZZ"
    "rejects a malformed escape in URL decoration",
    "https://github.com/owner/repository/pull/1?view=%ZZ"
    "rejects incomplete escaped UTF-8", "https://github.com/owner/repository/pull/%E0%A4%A"
    "rejects a Unicode pull request digit", "https://github.com/owner/repository/pull/%D9%A1"
]

let githubPullRequestUrlParsing =
    testList "GitHub.tryParsePullRequestUrl" [
        testCase
            "parses a standard GitHub pull request URL"
            (fun _ ->
                expectPullRequestInfo "https://github.com/TheAngryByrd/paket-lock-diff/pull/123" {
                    Owner = "TheAngryByrd"
                    Repository = "paket-lock-diff"
                    Number = "123"
                }
            )

        testCase
            "accepts URL decoration and a pull request sub-page"
            (fun _ ->
                expectPullRequestInfo
                    "https://www.github.com/fsprojects/Paket/pull/42/files/?diff=split#discussion_r1"
                    {
                        Owner = "fsprojects"
                        Repository = "Paket"
                        Number = "42"
                    }
            )

        testCase
            "accepts an HTTP GitHub URL without confusing its port for a repository segment"
            (fun _ ->
                expectPullRequestInfo "http://github.com:80/owner/repository_name/pull/7/commits" {
                    Owner = "owner"
                    Repository = "repository_name"
                    Number = "7"
                }
            )

        testCase
            "parses a GitHub pull request patch URL"
            (fun _ ->
                expectPullRequestInfo "https://github.com/owner/repository/pull/123.patch" {
                    Owner = "owner"
                    Repository = "repository"
                    Number = "123"
                }
            )

        testCase
            "parses a GitHub pull request diff URL"
            (fun _ ->
                expectPullRequestInfo "https://github.com/owner/repository/pull/456.diff" {
                    Owner = "owner"
                    Repository = "repository"
                    Number = "456"
                }
            )

        testList "invalid and edge-case URLs" [
            for name, url in invalidPullRequestUrls do
                testCase name (fun _ -> expectInvalidPullRequestUrl url)
        ]
    ]

let all = testList "All" [ githubPullRequestUrlParsing ]

[<EntryPoint>]
let main _ = Mocha.runTests all
