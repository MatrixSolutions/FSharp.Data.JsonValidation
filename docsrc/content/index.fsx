(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Data.JsonValidation/"
#r "../../bin/FSharp.Data.JsonValidation/FSharp.Data.JsonValidation.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

(**
FSharp.Data.JsonValidation
======================

A nifty little DSL for validating that JSON matches an expected schema.

<div class="row">
  <div class="span6">
    <div class="well well-small" id="nuget">
      FSharp.Data.JsonValidation can be <a href="https://nuget.org/packages/FSharp.Data.JsonValidation">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Data.JsonValidation</pre>
    </div>
  </div>
</div>

Example
-------

Validating that a JSON value is exactly `"hi"` or `42`
*)
#r "FSharp.Data.JsonValidation.dll"

open FSharp.Data
open JsonValidation

let schema = ExactlyOneOf [JsonValue.String "hi"; JsonValue.Number 42M]

validate schema (JsonValue.String "goodbye")
// returns: Invalid ""Expected value to be one of [\"hi\"; 42] but was \"goodbye\"\""

validate schema (JsonValue.Number 42M)
// returns: Valid


(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](api.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharp.Data.JsonValidation/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Data.JsonValidation
  [issues]: https://github.com/fsprojects/FSharp.Data.JsonValidation/issues
  [readme]: https://github.com/fsprojects/FSharp.Data.JsonValidation/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Data.JsonValidation/blob/master/LICENSE.txt
*)
