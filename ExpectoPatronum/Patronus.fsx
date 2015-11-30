#I "bin/Release"
#r "Fuchu.dll"
#load "Assert.fs"
open Fuchu

Assert.throws <| fun _ -> Assert.isNone (Some 2)
Assert.isNone None
Assert.throws <| fun _ -> Assert.isNonef (Some 1) ", to hold dear %i"
Assert.throwsc <| fun _ -> Assert.isNone (Some 1)
               <| fun err -> ()

Assert.throws <| fun _ -> Assert.isChoice1Of2 (Choice2Of2 2)
Assert.isChoice1Of2 (Choice1Of2 3)

Assert.throws <| fun _ -> Assert.isChoice2Of2 (Choice1Of2 1)
Assert.isChoice2Of2 (Choice2Of2 111)

Assert.isNull null
Assert.throws <| fun _ -> Assert.isNull (box 2)
Assert.throws <| fun _ -> Assert.isNullf (box 2) "expected null, got %O"
Assert.isNotNull (box 2)
Assert.throws <| fun _ -> Assert.isNotNull null
