module Shared.Tests

open Expecto

open Shared

let shared =
    testList "Shared" [
        testCase "PaketLocks.create preserves both lock files"
        <| fun _ ->
            let actual = PaketLocks.create "older contents" "newer contents"

            Expect.equal
                actual.OlderLockFile
                "older contents"
                "The older lock file should be preserved"

            Expect.equal
                actual.NewerLockFile
                "newer contents"
                "The newer lock file should be preserved"
    ]
