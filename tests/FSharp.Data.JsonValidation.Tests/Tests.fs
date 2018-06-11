module FSharp.Data.JsonValidation.Tests

open FSharp.Data
open FSharp.Data.JsonValidation
open NUnit.Framework
open System

let valid = function
  | Valid _ -> ()
  | result -> Assert.Fail (sprintf "Expected valid, got %A" result)

let invalid = function
  | Invalid _ -> ()
  | result -> Assert.Fail (sprintf "Expected invalid, got %A" result)

[<Test>]
let ``the docs example works as written (a fat test, but bad code in docs is annoying)`` () =
   let schema = ExactlyOneOf [JsonValue.String "hi"; JsonValue.Number 42M]
   let value = JsonValue.String "goodbye"

   Assert.AreEqual("Invalid \"Expected value to be one of [\"hi\"; 42] but was \"goodbye\"\"", sprintf "%A" (validate schema value))

   valid <| validate schema (JsonValue.Number 42M)

[<Test>]
let ``the tutorial example works as written (a fat test, but bad code in docs is annoying)`` () =
  let someNumbers = ArrayWhose [ItemsMatch AnyNumber]

  let looksLikeAnEmail (value: string) =
    value.Contains "@" && value.Contains "." && value.Length > 3


  let person =
    ObjectWhere
      [
        "name" .= StringThat [IsNotEmpty]
        "favoriteNumbers" .= someNumbers
        "email" .?= StringThat [IsEmail]
      ]

  let okayPerson =
    """
    {
      "name": "Saul Goodman",
      "favoriteNumbers": [747, 737000]
    }
    """

  valid <| validate person (JsonValue.Parse okayPerson)

  let emptyNamePerson =
    """
    {
      "name": "",
      "favoriteNumbers": [],
      "email": "mysterious@haunted.house"
    }
    """

  Assert.AreEqual("Invalid \".name Expected string to not be empty\"",  sprintf "%A" <| validate person (JsonValue.Parse emptyNamePerson))


  let badEmail =
    """
    {
      "name": "Jimmy M.",
      "favoriteNumbers": [],
      "email": "not-really-an-email"
    }
    """

  Assert.AreEqual("Invalid \".email Expected not-really-an-email to be a valid email address\"", sprintf "%A" <| validate person (JsonValue.Parse badEmail))

[<Test>]
let ``Anything matches anything`` () =
  valid <| validate Anything (JsonValue.String "hi")
  valid <| validate Anything (JsonValue.Number 42M)
  valid <| validate Anything (JsonValue.Array [||])
  valid <| validate Anything (JsonValue.Record [||])

[<Test>]
let ``Not only validates when the given schema doesn't`` () =
  invalid <| validate (Not AnyString) (JsonValue.String "hi")
  valid <| validate (Not AnyString) (JsonValue.Number 42M)
  valid <| validate (Not AnyString) (JsonValue.Array [||])
  valid <| validate (Not AnyString) (JsonValue.Record [||])

[<Test>]
let ``Not negates itself`` () =
  valid <| validate (Not (Not AnyString)) (JsonValue.String "hi")
  invalid <| validate (Not (Not AnyString)) (JsonValue.Number 42M)
  invalid <| validate (Not (Not AnyString)) (JsonValue.Array [||])
  invalid <| validate (Not (Not AnyString)) (JsonValue.Record [||])

[<Test>]
let ``Exactly only matches exactly what it's given`` () =
  valid <| validate (Exactly <| JsonValue.String "hi") (JsonValue.String "hi")
  invalid <| validate (Exactly <| JsonValue.String "hi") (JsonValue.String "hi2")
  invalid <| validate (Exactly <| JsonValue.String "hi") JsonValue.Null

[<Test>]
let ``ExactlyOneOf matches any of the values it's given`` () =
  let schema = ExactlyOneOf [JsonValue.String "hi"; JsonValue.Number 42M]

  valid <| validate schema (JsonValue.String "hi")
  valid <| validate schema (JsonValue.Number 42M)
  invalid <| validate schema JsonValue.Null
  invalid <| validate schema (JsonValue.String "hi2")

[<Test>]
let ``AnythingBut matches anything except for the literals given`` () =
  let schema = AnythingBut [JsonValue.String "hi"; JsonValue.Number 42M]

  invalid <| validate schema (JsonValue.String "hi")
  invalid <| validate schema (JsonValue.Number 42M)
  valid <| validate schema JsonValue.Null
  valid <| validate schema (JsonValue.String "hi2")

[<Test>]
let ``a JSON number is AnyNumber`` () =
  valid <| validate AnyNumber (JsonValue.Number 42M)

[<Test>]
let ``something other than a JSON number is not AnyNumber`` () =
  invalid <| validate AnyNumber (JsonValue.String "Foo")

[<Test>]
let ``a positive JSON number IsPositive`` () =
  valid <| validate (NumberThat [IsPositive]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsPositive]) (JsonValue.Number 0M)
  invalid <| validate (NumberThat [IsPositive]) (JsonValue.Number -42M)

[<Test>]
let ``a negative JSON number IsNegative`` () =
  invalid <| validate (NumberThat [IsNegative]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsNegative]) (JsonValue.Number 0M)
  valid <| validate (NumberThat [IsNegative]) (JsonValue.Number -42M)

[<Test>]
let ``a non-negative JSON number IsNonNegative`` () =
  valid <| validate (NumberThat [IsNonNegative]) (JsonValue.Number 42M)
  valid <| validate (NumberThat [IsNonNegative]) (JsonValue.Number 0M)
  invalid <| validate (NumberThat [IsNonNegative]) (JsonValue.Number -42M)

[<Test>]
let ``an integral JSON number IsIntegral`` () =
  valid <| validate (NumberThat [IsIntegral]) (JsonValue.Number 42M) 
  valid <| validate (NumberThat [IsIntegral]) (JsonValue.Number 0M)
  valid <| validate (NumberThat [IsIntegral]) (JsonValue.Number -42M)
  invalid <| validate (NumberThat [IsIntegral]) (JsonValue.Number 42.94M)
  invalid <| validate (NumberThat [IsIntegral]) (JsonValue.Number -42.94M)
  valid <| validate (NumberThat [IsIntegral]) (JsonValue.Float 0.0)

[<Test>]
let ``a JSON number that is greater than another number IsGreaterThan`` () =
  valid <| validate (NumberThat [IsGreaterThan 10M]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsGreaterThan 10M]) (JsonValue.Number 0M)
  invalid <| validate (NumberThat [IsGreaterThan 10M]) (JsonValue.Number 10M)

[<Test>]
let ``a JSON number that is less than another number IsLessThan`` () =
  invalid <| validate (NumberThat [IsLessThan 10M]) (JsonValue.Number 42M)
  valid <| validate (NumberThat [IsLessThan 10M]) (JsonValue.Number 0M)
  invalid <| validate (NumberThat [IsLessThan 10M]) (JsonValue.Number 10M)

[<Test>]
let ``a JSON number can have multiple NumberThat properties`` () =
  valid <| validate (NumberThat [IsGreaterThan 0M; IsPositive; IsIntegral]) (JsonValue.Number 42M)
  valid <| validate (NumberThat [IsGreaterThan 0M; IsPositive; IsNonNegative]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsLessThan 0M; IsNegative]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsNegative; IsPositive]) (JsonValue.Number 42M)
  invalid <| validate (NumberThat [IsNonNegative; IsGreaterThan -1M; IsNegative]) (JsonValue.Number 0M)

[<Test>]
let ``a JSON float will validate against NumberThat properties`` () = 
  valid <| validate (NumberThat [IsGreaterThan 0M; IsPositive]) (JsonValue.Float 42.5)
  valid <| validate (NumberThat [IsGreaterThan 0M; IsPositive; IsNonNegative]) (JsonValue.Float 42.5)
  invalid <| validate (NumberThat [IsLessThan 0M; IsNegative]) (JsonValue.Float 42.5)
  invalid <| validate (NumberThat [IsNegative; IsPositive]) (JsonValue.Float 42.5)
  invalid <| validate (NumberThat [IsNonNegative; IsGreaterThan -1M; IsNegative]) (JsonValue.Float 0.0)

[<Test>]
let ``a JSON string is AnyString`` () =
  valid <| validate AnyString (JsonValue.String "Hi")

[<Test>]
let ``something other than a JSON string is not AnyString`` () =
  invalid <| validate AnyString (JsonValue.Number 42M)

[<Test>]
let ``StringThat [IsNotEmpty] matches non-empty strings`` () =
  valid <| validate (StringThat [IsNotEmpty]) (JsonValue.String "hi")
  invalid <| validate (StringThat [IsNotEmpty]) (JsonValue.String "")

[<Test>]
let ``AnyObject works`` () =
  let schema = AnyObject

  valid <| validate schema (JsonValue.Record [||])
  invalid <| validate schema (JsonValue.Number 42M)
  valid <| validate schema (JsonValue.Record [| "foo", JsonValue.String "bar" |])

[<Test>]
let ``ObjectWhere [...] must have required keys that match the given schema`` () =
  let schema = ObjectWhere [ "foo" .= AnyString ]

  invalid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)
  valid <| validate schema (JsonValue.Parse """ { "foo": "bar" } """)

[<Test>]
let ``ObjectWhere [...] can omit optional keys`` () =
  let schema = ObjectWhere [ "foo" .?= AnyString ]

  valid <| validate schema (JsonValue.Parse """ { } """)
  valid <| validate schema (JsonValue.Parse """ { "foo": "bar" } """)

[<Test>]
let ``ObjectWhere [...] must meet the value schema when optional keys are given`` () =
  let schema = ObjectWhere [ "foo" .?= AnyString ]

  invalid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)


[<Test>]
let ``ObjectWhere [...] ignores keys that aren't mentioned in the schema`` () =
  let schema = ObjectWhere []

  valid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)


[<Test>]
let ``ArrayWhose [LengthIsAtLeast n] works`` () =
  let schema = ArrayWhose [LengthIsAtLeast 4]

  invalid <| validate schema (JsonValue.Array <| Array.replicate 3 JsonValue.Null)
  valid <| validate schema (JsonValue.Array <| Array.replicate 4 JsonValue.Null)
  valid <| validate schema (JsonValue.Array <| Array.replicate 5 JsonValue.Null)

[<Test>]
let ``AnyArray works`` () =
  let schema = AnyArray

  valid <| validate schema (JsonValue.Array [||])
  invalid <| validate schema (JsonValue.Number 42M)
  valid <| validate schema (JsonValue.Array [|JsonValue.String "foo"; JsonValue.Number 432M|])

[<Test>]
let ``ArrayWhose [LengthIsAtMost n] works`` () =
  let schema = ArrayWhose [LengthIsAtMost 4]

  valid <| validate schema (JsonValue.Array <| Array.replicate 3 JsonValue.Null)
  valid <| validate schema (JsonValue.Array <| Array.replicate 4 JsonValue.Null)
  invalid <| validate schema (JsonValue.Array <| Array.replicate 5 JsonValue.Null)

[<Test>]
let ``ArrayWhose [LengthIsExactly n] works`` () =
  let schema = ArrayWhose [LengthIsExactly 4]

  invalid <| validate schema (JsonValue.Array <| Array.replicate 3 JsonValue.Null)
  valid <| validate schema (JsonValue.Array <| Array.replicate 4 JsonValue.Null)
  invalid <| validate schema (JsonValue.Array <| Array.replicate 5 JsonValue.Null)

[<Test>]
let ``StringThat [IsLowerCase] works`` () =
  let schema = StringThat [IsLowerCase]

  valid <| validate schema (JsonValue.String "this is a test")
  invalid <| validate schema (JsonValue.String "THIS IS A TEST")
  invalid <| validate schema (JsonValue.String "This is a test")
  invalid <| validate schema (JsonValue.String "this is a tesT")

  invalid <| validate (StringThat [IsLowerCase; IsUpperCase]) (JsonValue.String "this is a test")

[<Test>]
let ``StringThat [IsUpperCase] works`` () =
  let schema = StringThat [IsUpperCase]

  invalid <| validate schema (JsonValue.String "this is a test")
  valid <| validate schema (JsonValue.String "THIS IS A TEST")
  invalid <| validate schema (JsonValue.String "This is a test")
  invalid <| validate schema (JsonValue.String "this is a tesT")

  invalid <| validate (StringThat [IsLowerCase; IsUpperCase]) (JsonValue.String "this is a test")

[<Test>]
let ``StringThat [MatchesCaseInsensitive] works`` () =
  let schema = StringThat [MatchesCaseInsensitive "this is a test"]

  valid <| validate schema (JsonValue.String "this is a test")
  valid <| validate schema (JsonValue.String "THIS IS A TEST")
  valid <| validate schema (JsonValue.String "tHiS iS a TeSt")

  invalid <| validate schema (JsonValue.String "Completely different string")

[<Test>]
let ``StringThat [IsEmail] works`` () =
  let schema = StringThat [IsEmail]
 
  valid <| validate schema (JsonValue.String "test@example.com")
  invalid <| validate schema (JsonValue.String "hello")
  invalid <| validate schema (JsonValue.String "test@com")
  invalid <| validate schema (JsonValue.String "test.com")
  invalid <| validate schema (JsonValue.String "@.com")

[<Test>] 
let ``StringThat [IsAlphaNumeric] works`` () =
  let schema = StringThat [IsAlphaNumeric]

  valid <| validate schema (JsonValue.String "12345")
  valid <| validate schema (JsonValue.String "foobar")
  valid <| validate schema (JsonValue.String "foo123bar456")
  
  invalid <| validate schema (JsonValue.String String.Empty) 
  invalid <| validate schema (JsonValue.String "foo bar")
  invalid <| validate schema (JsonValue.String "foo_bar")
  invalid <| validate schema (JsonValue.String "H3LL0 W0RLD!")
  invalid <| validate schema (JsonValue.String "@(*#@(")

[<Test>]
let ``StringThat [IsGuid] works`` () = 
  let schema = StringThat [IsGuid]
  let guid = Guid.NewGuid()

  valid <| validate schema (JsonValue.String <| string guid)
  invalid <| validate schema (JsonValue.String "foo bar")

[<Test>]
let ``StringThat [IsMinimumLength] works`` () =
  let schema n = StringThat [IsMinimumLength n]

  valid <| validate (schema 7) (JsonValue.String "foo bar")
  invalid <| validate (schema 8) (JsonValue.String "foo bar")
  valid <| validate (schema 6) (JsonValue.String "foo bar")
  invalid <| validate (schema -2) (JsonValue.String "foo bar")

[<Test>]
let ``StringThat [isMaximumLength] works`` () =
  let schema n = StringThat [IsMaximumLength n]

  valid <| validate (schema 7) (JsonValue.String "foo bar")
  valid <| validate (schema 8) (JsonValue.String "foo bar")
  invalid <| validate (schema 6) (JsonValue.String "foo bar")
  invalid <| validate (schema -2) (JsonValue.String "foo bar") 

[<Test>]
let ``ArrayWhose [ItemsMatch schema] ensures items all match`` () =
  let schema = ArrayWhose [ItemsMatch AnyNumber]

  invalid <| validate schema (JsonValue.Array [|JsonValue.Null|])
  invalid <| validate schema (JsonValue.Array [|JsonValue.Number 42M; JsonValue.Null|])
  valid <| validate schema (JsonValue.Array [|JsonValue.Number 42M; JsonValue.Number 0M|])

[<Test>]
let ``when ArrayWhose [ItemsMatch schema] fails, it reports on the index of failure`` () =
  let schema =
    ObjectWhere
      [
        "b" .= ArrayWhose [ItemsMatch AnyNumber]
      ]
  
  let result = validate schema (JsonValue.Parse """{ "b": [1, 2, "foo", 4] }""")

  match result with
    | Valid -> failwithf "Expected invalid"
    | Invalid message -> Assert.That(message, Is.StringContaining ".b [2]")

[<Test>]
let ``Either [...] ensures any item matches`` () =
  let schema = Either [Exactly <| JsonValue.Number 42M; AnyString]

  valid <| validate schema (JsonValue.String "42")
  invalid <| validate schema (JsonValue.Number 99M)
  valid <| validate schema (JsonValue.Number 42M)

[<Test>]
let ``Delay matches allowing recursive data structures`` () =
  let rec memory =
    ObjectWhere
      [
        "subject" .= StringThat [IsNotEmpty]
        "linkedMemory" .?= Delay (fun () -> memory)
      ]

  valid <| validate memory (JsonValue.Parse """ { "subject": "Lone Memory" } """)

  """
  {
    "subject": "I link",
    "linkedMemory": {
      "subject": "I'm linked to"
    }
  }
  """
    |> JsonValue.Parse
    |> validate memory
    |> valid

  """
  {
    "subject": "I link",
    "linkedMemory": {
      "subject": 42
    }
  }
  """
    |> JsonValue.Parse
    |> validate memory
    |> invalid