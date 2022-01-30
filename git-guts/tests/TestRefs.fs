namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open git_guts

// --------------------------------------------------------------------

[<TestClass>]
type TestRefs () =

    [<TestInitialize>]
    member this.init () =
        // prepare to run a test
        disableSpectreCapabilities

    [<TestMethod>]
    member this.TestDumpRefs () =

        let doTest zipFname =

            // set up the test repo
            use gitTestRepo = new GitTestRepo( zipFname )

            // dump the refs
            using ( new CaptureStdout() ) ( fun cap ->
                dumpRefs gitTestRepo.repoDir
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".refs.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                cap.checkOutput expectedFname
            )

            // move the loose objects to a pack, and check again
            runGitGc gitTestRepo.repoDir
            using ( new CaptureStdout() ) ( fun cap ->
                dumpRefs gitTestRepo.repoDir
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".refs-packed.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                cap.checkOutput expectedFname
            )

        // run the tests
        doTest "empty.zip"
        doTest "full2.zip"
