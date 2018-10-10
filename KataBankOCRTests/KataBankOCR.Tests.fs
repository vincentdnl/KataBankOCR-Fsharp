module KataBankOCRCore.Tests

open Expecto

[<Tests>]
let tests =
  testList "use case 1" [
    testCase "000000000" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                   | || || || || || || || || |\n\
                   |_||_||_||_||_||_||_||_||_|\n"
      let result = getAccountNumber zeros
      Expect.equal result "000000000" "All zeros test failed"

    testCase "111111111" <| fun _ ->
      let zeros = "                           \n\
                     |  |  |  |  |  |  |  |  |\n\
                     |  |  |  |  |  |  |  |  |\n"
      let result = getAccountNumber zeros
      Expect.equal result "111111111" "All ones test failed"

    testCase "222222222" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                    _| _| _| _| _| _| _| _| _|\n\
                   |_ |_ |_ |_ |_ |_ |_ |_ |_ \n"
      let result = getAccountNumber zeros
      Expect.equal result "222222222" "All twos test failed"

    testCase "333333333" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                    _| _| _| _| _| _| _| _| _|\n\
                    _| _| _| _| _| _| _| _| _|\n"
      let result = getAccountNumber zeros
      Expect.equal result "333333333" "All threes test failed"

    testCase "444444444" <| fun _ ->
      let zeros = "                           \n\
                   |_||_||_||_||_||_||_||_||_|\n\
                     |  |  |  |  |  |  |  |  |\n"
      let result = getAccountNumber zeros
      Expect.equal result "444444444" "All fours test failed"

    testCase "555555555" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                   |_ |_ |_ |_ |_ |_ |_ |_ |_ \n\
                    _| _| _| _| _| _| _| _| _|\n"
      let result = getAccountNumber zeros
      Expect.equal result "555555555" "All fives test failed"

    testCase "666666666" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                   |_ |_ |_ |_ |_ |_ |_ |_ |_ \n\
                   |_||_||_||_||_||_||_||_||_|\n"
      let result = getAccountNumber zeros
      Expect.equal result "666666666" "All sixs test failed"

    testCase "777777777" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                     |  |  |  |  |  |  |  |  |\n\
                     |  |  |  |  |  |  |  |  |\n"
      let result = getAccountNumber zeros
      Expect.equal result "777777777" "All sevens test failed"

    testCase "888888888" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                   |_||_||_||_||_||_||_||_||_|\n\
                   |_||_||_||_||_||_||_||_||_|\n"
      let result = getAccountNumber zeros
      Expect.equal result "888888888" "All heights test failed"

    testCase "999999999" <| fun _ ->
      let zeros = " _  _  _  _  _  _  _  _  _ \n\
                   |_||_||_||_||_||_||_||_||_|\n\
                    _| _| _| _| _| _| _| _| _|\n"
      let result = getAccountNumber zeros
      Expect.equal result "999999999" "All nines test failed"

    testCase "123456789" <| fun _ ->
      let zeros = "    _  _     _  _  _  _  _ \n\
                     | _| _||_||_ |_   ||_||_|\n\
                     ||_  _|  | _||_|  ||_| _|\n"
      let result = getAccountNumber zeros
      Expect.equal result "123456789" "All nines test failed"
  ]
