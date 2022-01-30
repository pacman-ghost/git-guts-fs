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
            let packDataFname = fnames.[0]

            // dump the pack data file
            using ( new CaptureStdout() ) ( fun cap ->
                dumpPackFile packDataFname
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".pack-data.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                cap.checkOutput expectedFname
            )

            // dump the pack index file
            let packIndexFname = changeExtn packDataFname ".idx"
            using ( new CaptureStdout() ) ( fun cap ->
                dumpPackFile packIndexFname
                let expectedFname =
                    let fname = Path.GetFileNameWithoutExtension( zipFname ) + ".pack-index.txt"
                    Path.Combine( __SOURCE_DIRECTORY__, "fixtures", fname )
                cap.checkOutput expectedFname
            )

            // check that we can find each object name correctly
            using ( new FileStream( packIndexFname, FileMode.Open, FileAccess.Read, FileShare.Read ) ) ( fun inp ->
                inp.Seek( int64( 4 + 4 + 4*256 - 4 ), SeekOrigin.Begin ) |> ignore
                let nObjs = readNboInt4 inp
                for objNo = 0 to nObjs-1 do
                    let objName, _, _ = readPackIndexObject inp objNo nObjs
                    let obj = findRepoObject gitTestRepo.repoDir objName
                    Assert.IsTrue( obj.IsSome )
            )

        // run the tests
        doTest "simple.zip"
        doTest "license.zip"
        doTest "full.zip"
        doTest "full2.zip"
