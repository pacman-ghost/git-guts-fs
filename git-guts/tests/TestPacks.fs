namespace tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open git_guts

// --------------------------------------------------------------------

[<TestClass>]
type TestPacks () =

    [<TestInitialize>]
    member this.init () =
        // prepare to run a test
        disableSpectreCapabilities

    [<TestMethod>]
    member this.TestDumpPack () =

        let doTest zipFname =

            // set up the test repo
            use gitTestRepo = new GitTestRepo( zipFname )
            runGitGc gitTestRepo.repoDir

            // locate the pack data file
            let fnames = findRepoPacks gitTestRepo.repoDir
            Assert.AreEqual( 1, fnames.Length )
            let packFname = fnames.[0]

            // dump the pack data file
            use cap = new CaptureStdout()
            dumpPackFile packFname
            let expectedFname =
                let fname = Path.GetFileNameWithoutExtension( zipFname ) + "-pack.txt"
                Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
            cap.checkOutput expectedFname

        // run the tests
        doTest "simple.zip"
