namespace Shared

open System

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

type PaketLocks = {
    OlderLockFile : string
    NewerLockFile : string
}

module PaketLocks =
    let create (olderLockFile : string) (newerLockFile : string) =
        {
            OlderLockFile = olderLockFile
            NewerLockFile = newerLockFile
        }

type Package = {
    GroupName : string
    PackageName : string
    Version : string
}
type PackageVersionDiff = {
    GroupName : string
    PackageName : string
    OlderVersion : string
    NewerVersion : string
}

type PaketDiff = {
    Additions : Package list
    Removals : Package list
    VersionIncreases : PackageVersionDiff list
    VersionDecreases : PackageVersionDiff list
}

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    {
        getTodos: unit -> Async<Todo list>
        addTodo: Todo -> Async<Todo>
        comparePaketLocks: PaketLocks -> Async<PaketDiff>
    }
