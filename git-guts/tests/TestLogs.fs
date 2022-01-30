namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open git_guts

// --------------------------------------------------------------------

[<TestClass>]
type TestLogs () =

    [<TestInitialize>]
    member this.init () =
        // prepare to run a test
        disableSpectreCapabilities ()

    [<TestMethod>]
    member this.TestDumpLogs () =

        let doTest zipFname =

            // set up the test repo
            use gitTestRepo = new GitTestRepo( zipFname )

            // dump the logs
            using ( new CaptureStdout() ) ( fun cap ->
                dumpLogs gitTestRepo.repoDir
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".logs.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                checkCapturedOutput cap expectedFname
            )

        // run the tests
        doTest "empty.zip"
        doTest "simple.zip"
        doTest "full2.zip"
