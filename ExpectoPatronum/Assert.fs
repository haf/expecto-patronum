module Fuchu.Assert

open Fuchu

let throws f =
  try
    f ()
    Tests.failtest "expected f to throw"
  with e ->
    ()

let throwsc f cont =
  try
    f ()
    Tests.failtest "expected f to throw"
  with e ->
    cont e

let isNone = function
  | None -> ()
  | Some x -> Tests.failtestf "expected None, was Some(%A)" x

let isNonef value format =
  match value with
  | None -> ()
  | Some x -> Printf.kprintf Tests.failtest format x

let isSome = function
  | None -> Tests.failtestf "expected Some _, was None"
  | Some _ -> ()

let isChoice1Of2 = function
  | Choice1Of2 _ -> ()
  | Choice2Of2 x -> Tests.failtestf "expected Choice1Of2, was Choice2Of2(%A)" x

let isChoice1Of2f value format =
  match value with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x -> Printf.kprintf Tests.failtest format x

let isChoice2Of2 = function
  | Choice1Of2 x -> Tests.failtestf "expected Choice2Of2 _, was Choice1Of2(%A)" x
  | Choice2Of2 _ -> ()

let isChoice2Of2f value format =
  match value with
  | Choice1Of2 x -> Printf.kprintf Tests.failtest format x
  | Choice2Of2 _ -> ()

let isNotNull = function
  | null -> Tests.failtest "expected not null, but was null"
  | x -> ()

let isNull = function
  | null -> ()
  | x -> Tests.failtestf "expected null, but was %A" x

let isNullf value format =
  match value with
  | null -> ()
  | x -> Printf.kprintf Tests.failtest format x

let isLessThan a b msg =
  if a < b then ()
  else Tests.failtestf "Expected a (%A) to be less than b (%A)" a b

let isLessThanOrEqual a b msg =
  if a <= b then ()
  else Tests.failtestf "Expected a (%A) to be less than or equal to b (%A)" a b

let isGreaterThan a b msg =
  if a > b then ()
  else Tests.failtestf "Expected a (%A) to be greater than or equal to b (%A)" a b

let isGreaterThanOrEqual a b msg =
  if a >= b then ()
  else Tests.failtestf "Expected a (%A) to be greater than or equal to b (%A)" a b
