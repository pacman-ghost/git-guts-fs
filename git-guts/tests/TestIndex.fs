namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open git_guts

// --------------------------------------------------------------------

[<TestClass>]
type TestStagingIndex () =

    [<TestInitialize>]
    member this.init () =
        // prepare to run a test
        disableSpectreCapabilities

    [<TestMethod>]
    member this.TestDumpStagingIndex () =

        let doTest zipFname =

            // set up the test repo
            use gitTestRepo = new GitTestRepo( zipFname )
            runGitGc gitTestRepo.repoDir

            // dump the staging index
            using ( new CaptureStdout() ) ( fun cap ->
                dumpStagingIndex gitTestRepo.repoDir false
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".staging-index.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                cap.checkOutput expectedFname
            )

        // run the tests
        Assert.ThrowsException<Exception>( fun () ->
            doTest "empty.zip"
        ) |> ignore
        doTest "simple.zip"
