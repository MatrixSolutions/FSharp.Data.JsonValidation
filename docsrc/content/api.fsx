# API Documentation

## `validate`
The `validate` function checks a schema against JSON (FSharp.Data.JsonValue) returning
a `ValidationResult`.

```fsharp
let validate : JsonSchema -> JsonValue -> ValidationResult
```

A `ValidationResult` is simply defined as

```fsharp
type ValidationResult =
  | Valid
  | Invalid of string
```
See [the tutorial](tutorial.html) for a full example.

**Below are the many ways to define `JsonSchema`s.**


### Anything

#### A `JsonSchema` that validates against anything
Useful for those times when you just don't care, or no actual structure is expected.

Anything


### Exact Matches

#### Match _exactly_ a `JsonValue`
```fsharp
Exactly of JsonValue
```



Example: schema to match exactly `42`

```fsharp 
let schema42 = Exactly (JsonValue.Number 42M)
```

#### Match exactly one of a few possible `JsonValue`s
```fsharp
ExactlyOneOf of JsonValue list
```



Example: schema to match exactly `null` or `"the answer"`

```fsharp
let nullOrAnswer = ExactlyOneOf [JsonValue.Null; JsonValue.String "the answer"]
```

#### Match anything but certain `JsonValue`s
```fsharp
AnythingBut of JsonValue list
```

Example: schema to match anything _except_ for "foobar" or "spameggs"

```fsharp
let noFoobarOrSpameggs = AnythingBut [JsonValue.String "foobar"; JsonValue.String "spameggs"]
```

#### Match the opposite of a `JsonSchema`
```fsharp
Not of JsonSchema
```



Example: schema to match anything that's not a string
```fsharp
let nonString = Not AnyString
```

## Literals



### Numbers

#### Validate that JSON is any number

AnyNumber


#### Validate a number with specific criteria
```fsharp
NumberThat of NumberAttributes list
```


Where `NumberAttributes` can be any valid combination of

  - `IsPositive` - ensure that the number is positive (greater than 0).

  - `IsNegative` - ensure that the number is negative (less than 0).

  - `IsNonNegative` - ensure that the number is 0 or greater.

  - `IsIntegral` - ensure that the number is an integral number.

  - `IsGreaterThan` - ensure that the number is greater than a given number.

  - `IsLessThan` - ensure that the number is less than a given number.

Example: schema to match that a number is positive, greater than 10 and less than 50
```fsharp
let numberSchema = NumberThat [IsPositive; IsGreaterThan 10M; IsLessThan 50M]
```
### Strings

#### Validate that JSON is any string

AnyString


#### Validate a string with specific criteria
```fsharp
StringThat of StringAttributes list
```


Where `StringAttributes` can be any combination of:
  - `IsNotEmpty` - ensure the string has length
  - `IsLowerCase` - ensure the string contains all lowercase alphabetical characters 
  - `IsUpperCase` - ensure the string contains all uppercase alphabetical characters
  - `IsEmail` - ensure the string is a valid email address
  - `IsAlpaNumberic` - ensure the string contains only alpha numeric characters (no spaces or underscores)
  - `IsGuid` - ensure the string is a valid guid
  - `IsMinimumLength n` - ensure the string is NOT less than `n`
  - `IsMaximumLength n` - ensure the string is NOT greater than `n`
  - `MatchesCaseInsensitive str` - ensure that the string matches the given string, regardless of case
  - `MeetsCriteria of Description*Predicate` - ensure the string meets an arbitrary condition where

  - `Description` is a description of the criteria, useful for error messages

  - `Predicate` is the actual predicate `(string -> bool)` to check

Example: schema to match "long" strings that contain at least one dash
```fsharp
let longDashing = StringThat [MeetsCriteria ("long and dashing", fun s -> s.Length > 1000 && s.Contains "-")]
```

### Objects

#### Validate an object with specific keys
```fsharp
ObjectWhere of (KeyValidation * JsonSchema) list
```



Where `KeyValidation*JsonSchema` specifies key-value schemas such that

  - `KeyValidation` indicates required or optional (omittable) keys and

  - `JsonSchema` constrains the allowable value under the keys

Note that `( .= )` and `( .?= )` are super useful for defining object schemas where

  - `( .= )` indicates a required key whose value must abide the given schema and

  - `( .?= )` indicates an optional key (can be left out) whose value, if the key is given,
    must abide the given schema and

Example: schemas to match addresses and people who may or may not have an address
```fsharp
let address = 
  ObjectWhere
    [
      "street" .= StringThat [IsNotEmpty]
      "area" .= StringThat [IsNotEmpty]
      "postalCode" .= StringThat [IsNotEmpty]
    ]

let person =
  ObjectWhere
    [
      "name" .= StringThat [IsNotEmpty]
      "address" .?= address
    ]
```

### Arrays

#### Validate an array
```fsharp
ArrayWhose of ArrayAttributes list
```



Where `ArrayAttributes` can be any combination of

  - `LengthIsAtLeast of int` constrains the minimum length of the array

  - `LengthIsAtMost of int` constrains the maximum length of the array

  - `LengthIsExactly of int` constrains the exact length of the array (probably shouldn't be
    used with the two above)

  - `ItemsMatch of JsonSchema` specifies the schema of the array's items

Example: schema to match an array of exactly 42 numbers
```fsharp
let numbers42 = ArrayWhose  [LengthIsExactly 42; ItemsMatch AnyNumber]
```

### Alternatives

#### Given multiple schemas, validate that _any_ match
```fsharp
Either of JsonSchema list
```



Where `JsonSchema` can be any other `JsonSchema` described hereabouts

Example: schema to match either a string, number or `null`
```fsharp
let stringOrNumber = Either [AnyString; AnyNumber; Exactly JsonValue.Null]
```

### Laziness

#### Match a schema, lazily
Useful when your JSON types may contain themselves
```fsharp
Delay of (unit -> JsonSchema)
```



Where `unit -> JsonSchema` is a function that just returns another
`JsonSchema` to validate.

Example: schema to match a memory that may link to a memory that may link to a memory
that may lin...

```fsharp
let rec memory =
  ObjectWhere
    [
      "subject" .= StringThat [IsNotEmpty]
      "linkedMemory" .?= Delay (fun () -> memory)
    ]
```