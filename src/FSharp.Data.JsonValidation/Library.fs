namespace FSharp.Data

module public JsonValidation =
  open System

  type ValidationResult =
    | Valid
    | Invalid of string

  type KeyValidation =
    | Required of string
    | Optional of string
 
  type private Description = string
  type private Predicate = string->bool
  
  type StringAttributes =
    | IsNotEmpty
    | IsLowerCase
    | IsUpperCase
    | IsEmail
    | IsAlphaNumeric
    | IsGuid
    | IsMinimumLength of int
    | IsMaximumLength of int
    | MatchesCaseInsensitive of string
    | MeetsCriteria of Description*Predicate

  type NumberAttributes =
    | IsPositive
    | IsNegative
    | IsNonNegative
    | IsIntegral
    | IsGreaterThan of decimal
    | IsLessThan of decimal

  type ArrayAttributes =
    | LengthIsAtLeast of int
    | LengthIsAtMost of int
    | LengthIsExactly of int
    | ItemsMatch of JsonSchema

  and JsonSchema =
    | AnyNumber
    | NumberThat of NumberAttributes list
    | AnyString
    | AnyArray
    | AnyObject
    | StringThat of StringAttributes list
    | ObjectWhere of (KeyValidation * JsonSchema) list
    | Either of JsonSchema list
    | ArrayWhose of ArrayAttributes list
    | Exactly of JsonValue
    | ExactlyOneOf of JsonValue list
    | AnythingBut of JsonValue list
    | Anything
    | Not of JsonSchema
    | Delay of (unit -> JsonSchema)
   
     
  let private (|PositiveInteger|NonPositiveInteger|) n =
    if n > -1
    then PositiveInteger
    else NonPositiveInteger

  let nonEmptyString = StringThat [IsNotEmpty]

  let inline (.=) key (schema: JsonSchema) = Required key, schema
  let inline (.?=) key (schema: JsonSchema) = Optional key, schema
  let private (!<) (num1: int) (num2: int) = not <| (num1 < num2)
  let private (!>) (num1: int) (num2: int) = not <| (num1 > num2)

  let inline (-->) choice1 getChoice2 =
    match choice1 with
      | Valid -> getChoice2 ()
      | Invalid error -> Invalid error

  let mapInvalid fn = function
    | Invalid v -> Invalid <| fn v
    | Valid -> Valid

  let private matchesRegex regex str =
    let r = Text.RegularExpressions.Regex(regex)
    r.IsMatch(str)

  let rec private stringMeetsProperties str props =
    match str, props with
      | _, [] -> Valid
      | "", (IsNotEmpty::_) -> Invalid "Expected string to not be empty"
      | str, (IsNotEmpty::rest) -> stringMeetsProperties str rest
      | str, (IsLowerCase::rest) -> isPropertyValid isLowerCase str rest "be completely lowercase"
      | str, (IsUpperCase::rest) -> isPropertyValid isUpperCase str rest "be completely uppercase"
      | str, (IsEmail::rest) -> isPropertyValid isEmail str rest "be a valid email address"
      | str, (IsAlphaNumeric::rest) -> isPropertyValid isAlphaNumeric str rest "be alpha numeric"
      | str, (IsGuid::rest) -> isPropertyValid isValidGuid str rest "be a valid Guid"
      | str, (IsMinimumLength len::rest) -> isPropertyValid (isMinimumLength len) str rest (sprintf "be at least %d" len)
      | str, (IsMaximumLength len::rest) -> isPropertyValid (isMaximumLength len) str rest (sprintf "be at most %d" len)
      | str, (MatchesCaseInsensitive givenStr::rest) -> isPropertyValid (caseInsensitive givenStr) str rest "be case insensitive" 
      | str, (MeetsCriteria (description, pred)::rest) -> isPropertyValid pred str rest description

  and private isPropertyValid f str rest description =
     match f str with 
      | true -> stringMeetsProperties str rest
      | false -> Invalid <| sprintf "Expected %s to %s" str description

  and private isLowerCase str = 
    str.ToLower() = str
  
  and private isUpperCase str = 
    str.ToUpper() = str

  and private caseInsensitive providedStr str =
    String.Compare(providedStr, str, true) = 0

  and private isEmail =
    matchesRegex "^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$"

  and private isAlphaNumeric =
    matchesRegex "^[a-zA-Z0-9]+$"

  and private isValidGuid =
    Guid.TryParse >> fst

  and private isMinimumLength len str =
    match len with
      | PositiveInteger -> (!<) str.Length len
      | NonPositiveInteger -> false

  and private isMaximumLength len str =
    match len with
      | PositiveInteger -> (!>) str.Length len
      | NonPositiveInteger -> false
    
  let rec private numberMeetsProperties num props =
    match num, props with
      | _, [] -> Valid
      | num, IsPositive::rest when num > 0M -> numberMeetsProperties num rest
      | num, IsNegative::rest when num < 0M -> numberMeetsProperties num rest
      | num, IsNonNegative::rest when num >= 0M -> numberMeetsProperties num rest
      | num, IsIntegral::rest when num % 1M = 0M -> numberMeetsProperties num rest
      | num, IsGreaterThan n::rest when num > n -> numberMeetsProperties num rest
      | num, IsLessThan n::rest when num < n -> numberMeetsProperties num rest
      | num, prop::_ -> Invalid <| sprintf "Expected number %f to meet %A property" num prop

  let private isExactlyOneOf literals value =
    match List.exists (fun expected -> expected = value) literals with
    | true ->  Valid
    | false -> Invalid <| sprintf "Expected value to be one of %A but was %A" literals value

  let private isNoneOf literals value =
    match List.tryFind (fun expected -> expected = value) literals with
    | None ->  Valid
    | Some value -> Invalid <| sprintf "Expected value to not be any of %A but was %A" literals value

  let private isExactly expected actual =
    match expected = actual with
    | true -> Valid
    | false -> Invalid <| sprintf "Expected value to be %A but was %A" expected actual

  let rec private gatherRequiredProperties = function
    | [] -> Map.empty
    | (Required prop, schema)::rest -> Map.add prop schema <| gatherRequiredProperties rest
    | _::rest -> gatherRequiredProperties rest

  let rec private gatherOptionalProperties = function
    | [] -> Map.empty
    | (Optional prop, schema)::rest -> Map.add prop schema <| gatherOptionalProperties rest
    | _::rest -> gatherOptionalProperties rest

  let private hasAll props (required: Map<string, 'a>) =
    let requiredKeys = Map.toList required |> List.map fst
    let keyExists key = Seq.exists (fun (prop, _) -> prop = key) props
    match Seq.forall keyExists requiredKeys with
    | true -> Valid
    | false -> Invalid <| sprintf "Expected object to have properties %A, but it has %A" (Map.toList required |> Seq.map fst) (props |> Seq.map fst)

  let rec validate (schema : JsonSchema) (json : JsonValue) =
    try
      match json, schema with
        | JsonValue.Float _, AnyNumber
        | JsonValue.Number _, AnyNumber -> Valid
        | JsonValue.Number n, NumberThat properties -> numberMeetsProperties n properties 
        | JsonValue.Float n, NumberThat properties -> numberMeetsProperties (decimal n) properties
        | invalid, AnyNumber
        | invalid, NumberThat _ -> Invalid <| sprintf "Expected a number, got %A" invalid

        | JsonValue.String _,  AnyString -> Valid
        | invalid, AnyString -> Invalid <| sprintf "Expected a string, got %A" invalid
        | JsonValue.String string, StringThat properties -> stringMeetsProperties string properties
        | invalid, StringThat _ -> Invalid <| sprintf "Expected a string, got %A" invalid

        | value, ExactlyOneOf literals -> isExactlyOneOf literals value
        | value, AnythingBut literals -> isNoneOf literals value
        | value, Exactly literal -> isExactly literal value

        | JsonValue.Record properties, ObjectWhere propSchema -> propertiesMeetSchema properties propSchema
        | invalid, ObjectWhere _ -> Invalid <| sprintf "Expected an object, got %A" invalid
        | JsonValue.Record _, AnyObject -> Valid
        | invalid, AnyObject -> Invalid <| sprintf "Expected an object, got %A" invalid

        | JsonValue.Array items, ArrayWhose properties -> arrayMeetsProperties items properties
        | invalid, ArrayWhose _ -> Invalid <| sprintf "Expected an array, got %A" invalid
        | JsonValue.Array _, AnyArray -> Valid
        | invalid, AnyArray -> Invalid <| sprintf "Expected an array, got %A" invalid

        | value, Either schemas -> validateEither value schemas

        | value, Not schema -> negateValidation schema value

        | _, Anything -> Valid

        | value, Delay getSchema -> validate (getSchema ()) value
    with
    | ex -> Invalid <| ex.Message

  and private negateValidation schema value =
    match validate schema value with
      | Valid -> Invalid <| sprintf "Expected %A not to match schema %A" value schema
      | Invalid _ -> Valid

  and private requiredValuesMeetSchemas (requiredLookup : Map<string, JsonSchema>) props  =
    let formatErrorUnder = sprintf ".%s %s"
    let validateEach result (key, value) =
      result --> fun () -> mapInvalid (formatErrorUnder key) <| validate requiredLookup.[key] value

    props
      |> Array.filter (requiredLookup.ContainsKey << fst)
      |> Array.fold validateEach Valid

  and private optionalValuesMeetSchemas (optionalLookup : Map<string, JsonSchema>) props =
    let formatErrorUnder = sprintf ".%s %s"
    let validateEach result (key, value) =
      result --> fun () -> mapInvalid (formatErrorUnder key) <| validate optionalLookup.[key] value

    props
      |> Array.filter (optionalLookup.ContainsKey << fst)
      |> Array.fold validateEach Valid

  and private propertiesMeetSchema properties propSchema =
    let requiredLookup = gatherRequiredProperties propSchema
    let optionalLookup = gatherOptionalProperties propSchema

    hasAll properties requiredLookup
      --> fun () -> requiredValuesMeetSchemas requiredLookup properties
      --> fun () -> optionalValuesMeetSchemas optionalLookup properties

  and private arrayMeetsProperties items props =
    match props with
      | [] -> Valid
      | LengthIsAtLeast n::rest ->
        if items.Length >= n then arrayMeetsProperties items rest
        else Invalid <| sprintf "Expected array to have at least %d item(s), but it had %d" n items.Length

      | LengthIsAtMost n::rest ->
        if items.Length <= n then arrayMeetsProperties items rest
        else Invalid <| sprintf "Expected array to have at most %d item(s), but it had %d" n items.Length

      | LengthIsExactly n::rest ->
        if items.Length = n then arrayMeetsProperties items rest
        else Invalid <| sprintf "Expected array to have exactly %d item(s), but it had %d" n items.Length

      | ItemsMatch schema::rest ->
        match validateAll 0 (Array.toList items) schema with
          | Valid -> arrayMeetsProperties items rest
          | Invalid error -> Invalid error

  and private validateAll i items schema =
    let formatErrorUnder = sprintf "[%d] %s"

    match items with
      | [] -> Valid
      | item::rest ->
        match validate schema item with
          | Valid -> validateAll (i + 1) rest schema
          | Invalid error -> mapInvalid (formatErrorUnder i) <| Invalid error

  and private validateEither value schemas  =
    let rec go errorAcc = function
      | [] -> errorAcc
      | schema::rest ->
        match validate schema value with
          | Valid -> []
          | Invalid error -> go (error::errorAcc) rest

    let errors = go [] schemas

    match errors with
      | [] -> Valid
      | invalids ->
        invalids
          |> String.concat " (or) "
          |> Invalid