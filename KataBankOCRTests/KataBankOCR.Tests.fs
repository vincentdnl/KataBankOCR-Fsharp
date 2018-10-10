module KataBankOCRCore.Tests

open Expecto

[<Tests>]
let tests =
  testList "Hello" [
    testCase "Hello World" <| fun _ ->
      Expect.equal hello "Hello World" "It's not Hello World..."
  ]
