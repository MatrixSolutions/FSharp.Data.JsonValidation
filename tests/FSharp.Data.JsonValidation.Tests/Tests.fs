module FSharp.Data.JsonValidation.Tests

open FSharp.Data
open FSharp.Data.JsonValidation
open Expecto

let valid = function
  | Valid _ -> ()
  | result -> failwith <| sprintf "Expected valid, got %A" result

let invalid = function 
  | Invalid _ -> ()
  | result -> failwith <| sprintf "Expected invalid, got %A" result 

let toJsonString str = JsonValue.String str  
let toJsonNumber num = num |> (JsonValue.Number << decimal)
let toNullJsonArray count = Array.replicate count JsonValue.Null |> JsonValue.Array
let toJsonArray array = JsonValue.Array array 

[<Tests>]
let tests =
  testList "FSharp.Data.JsonValidation tests" [
    
    testCase "the docs example works as written (a fat test, but bad code in docs is annoying)" <| fun _ -> 
      let schema = ExactlyOneOf [toJsonString "hi"; toJsonNumber 42]
      let value = toJsonString "goodbye"

      Expect.equal "Invalid \"Expected value to be one of [\"hi\"; 42] but was \"goodbye\"\"" (sprintf "%A" <| validate schema value) "there is an invalid result"

    testCase "the tutorial example works as written (a fat test, but bad code in docs is annnoying" <| fun _ -> 
      let someNumbers = ArrayWhose [ ItemsMatch AnyNumber ]
      let looksLikeAnEmail (value: string) = 
        value.Contains "@" && value.Contains "." && value.Length > 3

      let person =
        ObjectWhere 
          [
            "name" .= StringThat [ IsNotEmpty ]
            "favoriteNumbers" .= someNumbers
            "email" .?= StringThat [ MeetsCriteria ("be an email", looksLikeAnEmail) ]
          ]
      
      let okayPerson = """
        {
          "name": "Saul Goodman",
          "favoriteNumbers": [747, 737000]
        }"""

      valid <| validate person (JsonValue.Parse okayPerson)

      let emptyNamePerson = """
        {
          "name": "",
          "favoriteNumbers": [],
          "email": "mysterious@haunted.house"
        }"""
        
      let badEmail = """
        {
          "name": "Jimmy M.",
          "favoriteNumbers": [],
          "email": "not-really-an-email"
        }"""

      Expect.equal "Invalid \".name Expected string to not be empty\"" (sprintf "%A" <| validate person (JsonValue.Parse emptyNamePerson)) "expected string to not be empty"
      Expect.equal "Invalid \".email Expected string \"not-really-an-email\" to be an email\"" (sprintf "%A" <| validate person (JsonValue.Parse badEmail)) "expected to be an email string"
  
    testCase "anything matches anything" <| fun _ -> 
      valid <| validate Anything (toJsonString "hi")
      valid <| validate Anything (toJsonNumber 42)
      valid <| validate Anything (JsonValue.Array [||])
      valid <| validate Anything (JsonValue.Array [||])
  
    testCase "Exactly only matches exactly what it is given" <| fun _ ->
      valid <| validate (Exactly <| toJsonString "hi") (toJsonString "hi")
      invalid <| validate (Exactly <| toJsonString "hi") (toJsonString "hi2")
      invalid <| validate (Exactly <| toJsonString "hi") JsonValue.Null

    testCase "ExactlyOneOf matches any of the values that it is given" <| fun _ ->
      let schema = ExactlyOneOf [ toJsonString "hi"; toJsonNumber 42 ]

      valid <| validate schema (toJsonString "hi")
      valid <| validate schema (toJsonNumber 42)
      invalid <| validate schema JsonValue.Null
      invalid <| validate schema (toJsonString "hi2")

    testCase "a JSON Number is AnyNumber" <| fun _ -> 
      valid <| validate AnyNumber (toJsonNumber 42)

    testCase "something other than a JSON number is not AnyNumber" <| fun _ ->
      invalid <| validate AnyNumber (toJsonString "I am a string")

    testCase "a JSON string is AnyString" <| fun _ -> 
      valid <| validate AnyString (toJsonString "Hi")

    testCase "something other than a JSON string is not AnyString" <| fun _ ->
      invalid <| validate AnyString (toJsonNumber 42)

    testCase "StringThat [ IsNotEmpty ] matches non-empty strings" <| fun _ -> 
      valid <| validate (StringThat [ IsNotEmpty ]) (toJsonString "hi")
      invalid <| validate (StringThat [ IsNotEmpty ]) (toJsonString "")

    testCase "ObjectWhere [...] must have required keys that match the given schema" <| fun _ ->
      let schema = ObjectWhere [ "foo" .= AnyString ]

      invalid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)
      valid <| validate schema (JsonValue.Parse """ { "foo": "bar" } """)

    testCase "`ObjectWhere [...] can omit optional keys" <| fun _ -> 
      let schema = ObjectWhere [ "foo" .?= AnyString ]

      valid <| validate schema (JsonValue.Parse """ { } """)
      valid <| validate schema (JsonValue.Parse """ { "foo": "bar" } """)

    testCase "ObjectWhere [...] must meet the value schema when optional keys are given" <| fun _ -> 
      let schema = ObjectWhere [ "foo" .?= AnyString ]

      invalid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)

    testCase "ObjectWhere [...] ignores keys that aren't mentioned in the schema" <| fun _ -> 
      let schema = ObjectWhere []

      valid <| validate schema (JsonValue.Parse """ { "foo": 42 } """)

    testCase "ArrayWhose [LengthIsAtLeast n] works" <| fun _ -> 
      let schema = ArrayWhose [LengthIsAtLeast 4]

      invalid <| validate schema (toNullJsonArray 3)
      valid <| validate schema (toNullJsonArray 4)
      valid <| validate schema (toNullJsonArray 5)

    testCase "ArrayWhose [LengthIsAtMost n] works" <| fun _ -> 
      let schema = ArrayWhose [LengthIsAtMost 4]

      invalid <| validate schema (toNullJsonArray 3)
      valid <| validate schema (toNullJsonArray 4)
      valid <| validate schema (toNullJsonArray 5)
    
    testCase "ArrayWhose [LengthIsExactly n] works" <| fun _ -> 
      let schema = ArrayWhose [LengthIsExactly 4]

      invalid <| validate schema (toNullJsonArray 3)
      valid <| validate schema (toNullJsonArray 4)
      valid <| validate schema (toNullJsonArray 5)

    testCase "ArrayWhose [ItemsMatch schema] ensures items all match" <| fun _ ->
      let schema = ArrayWhose [ ItemsMatch AnyNumber ]

      invalid <| validate schema (toJsonArray [| JsonValue.Null |])
      invalid <| validate schema (toJsonArray [| toJsonNumber 42; JsonValue.Null |])
      valid <| validate schema (JsonValue.Array [| toJsonNumber 42; toJsonNumber 0 |])

    testCase "Either [...] ensures any item matches" <| fun _ -> 
      let schema = Either [ Exactly <| toJsonNumber 42; AnyString ]

      valid <| validate schema (toJsonString "42")
      invalid <| validate schema (toJsonNumber 99)
      valid <| validate schema (toJsonNumber 42)

    testCase "Delay matches allowing recursive data structures" <| fun _ ->
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
  ]
