namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open git_guts

// --------------------------------------------------------------------

[<TestClass>]
type TestVerify () =

    [<TestInitialize>]
    member this.init () =
        // prepare to run a test
        disableSpectreCapabilities ()

    [<TestMethod>]
    member this.TestVerify () =

        let doChecks repoDir =

            // verify retrieving objects from the repo
            using ( new CaptureStdout() ) ( fun cap ->
                verifyObjects repoDir false
            )

            // verify retrieving logs from the repo
            using ( new CaptureStdout() ) ( fun cap ->
                verifyLogs repoDir
            )

        let doTest zipFname =

            // set up the test repo
            use gitTestRepo = new GitTestRepo( zipFname )

            // do the checks
            doChecks gitTestRepo.repoDir

            // run garbage collection, and verify again
            runGitGc gitTestRepo.repoDir
            doChecks gitTestRepo.repoDir

        // run the tests
        let dname = Path.Combine( __SOURCE_DIRECTORY__, "fixtures" )
        for fname in Directory.GetFiles( dname, "*.zip", SearchOption.AllDirectories ) do
            doTest ( Path.GetFileName fname )
