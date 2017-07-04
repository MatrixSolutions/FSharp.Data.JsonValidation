(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Data.JsonValidation/"
#r "../../bin/FSharp.Data.JsonValidation/FSharp.Data.JsonValidation.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
let looksLikeAnEmail (value: string) =
  value.Contains "@" && value.Contains "." && value.Length > 3

(**
Introducing FSharp.Data.JsonValidation
========================

A nifty little DSL for validating that JSON matches an expected schema.

## First things first...

Let's include the library (use `#r` if you're writing an fsx), also include `FSharp.Data`
*)
#r "FSharp.Data.JsonValidation.dll"

open FSharp.Data
open JsonValidation

(**
## Define your schemas

Next define some schemas that describe how the JSON you're expecting should look.
Schemas are just values and can easily be combined.
*)


let someNumbers = ArrayWhose [ItemsMatch AnyNumber]

let person =
  ObjectWhere
    [
      "name" .= StringThat [IsNotEmpty]
      "favoriteNumbers" .= someNumbers
      "email" .?= StringThat [MeetsCriteria ("be an email", looksLikeAnEmail)]
    ]

(**
where `looksLikeAnEmail` is a silly little helper for illustrative purposes (i.e. you probably
don't want to use this in production code) that looks like

```fsharp
let looksLikeAnEmail (value: string) =
  value.Contains "@" && value.Contains "." && value.Length > 3
```

## Validate some JSON!

Lastly let's try some actual JSON!
*)

let okayPerson =
  """
  {
    "name": "Saul Goodman",
    "favoriteNumbers": [747, 737000]
  }
  """

validate person (JsonValue.Parse okayPerson)
// returns: Valid

let emptyNamePerson =
  """
  {
    "name": "",
    "favoriteNumbers": [],
    "email": "mysterious@haunted.house"
  }
  """

validate person (JsonValue.Parse emptyNamePerson)
// returns: Invalid 


let badEmail =
  """
  {
    "name": "Jimmy M.",
    "favoriteNumbers": [],
    "email": "not-really-an-email"
  }
  """

validate person (JsonValue.Parse badEmail)
// returns: Invalid ".email Expected string \"not-really-an-email\" to be an email\""

(**
## What's going on here?

We're defining two different schemas
```fsharp
let someNumbers = ...

let person = ...
```

`someNumbers` is a simple schema that describes a JSON array whose items are any number.
It will only be valid if, well, it's exactly that: an array containing number (or perhaps none).

`person` is a slightly more complicated schema. It says that a person must be a JSON object
that must have a `"name"` key whose value is a non-empty string. `( .= )`  indicates a required key
whose value matches another schema.

The `person` object must also have `"favoriteNumbers"` which can be `someNumbers` (here we're re-using
our other schema).

Finally, we're saying that `person` may have an `"email"` key with a value that meets some criteria.
`( .?= )` indicates an optional key and `MeetsCriteria` takes a description of the expectation and an
actual function (a predicate) to check whether it matches.
*)