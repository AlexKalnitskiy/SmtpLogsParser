namespace SmtpLogsParser.Tests

open FsCheck
open Microsoft.VisualStudio.TestTools.UnitTesting

module Test = 
    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

[<TestClass>]
type TestClass () =
  
    [<TestMethod>]
    member this.TestMethodPassing () =       
        Check.Quick Test.revRevIsOrig
