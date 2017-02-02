Elm NHS Number Validator
========================

Provides an NHS Number validator for String and (List Int) values.

    -- Returns a Result.Err due to invalid number
    NHSNumber.validateFromString "1234567890"

    NHSNumber.validateFromList [1,2,3,4,5,6,7,8,9,0]


To get valid NHS Numbers, there is a [valid nhs number generator](http://danielbayley.uk/nhs-number/).  You may also
refer to tests in this library.

### Tests

Tests are developed and run with the `elm-test` package from the `elm-community`.

### License

MIT, see `LICENSE.md`
