// Learn more about F# at http://fsharp.org

open System
open Shared

let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0"

type Parameters = (int*int*int) 

type Instruction = 
    | Add of Parameters
    | Mul of Parameters
    | Exit

type State = {
    Memory : Map<int, int>
    InstructionPointer : int
}

let getInstructions {Memory=p;InstructionPointer=i} =
    let ai = p.[i+1]
    let bi = p.[i+2]
    let a = p.[ai]
    let b = p.[bi]
    let t = p.[i+3]
    (a, b, t)

let parseOperation state =
    let {Memory=p;InstructionPointer=i} = state
    match p.[i] with
    | 1     -> Add <| getInstructions state
    | 2     -> Mul <| getInstructions state
    | 99    -> Exit
    | _     -> raise (ArgumentException "foo")

let applyAdd add  state =
    let {Memory=p} = state
    let (a,b,t) = add
    let sum = a + b
    let updatedMap = Map.add t sum p
    {state with Memory = updatedMap};

let applyMull add  state =
    let {Memory=p} = state
    let (a,b,t) = add
    let sum = a * b
    let updatedMap = Map.add t sum p
    {state with Memory = updatedMap};

let furtherIndex state = {state with InstructionPointer = state.InstructionPointer + 4}

let rec solveProgram state =
    match parseOperation state with
    | Add add -> applyAdd add state |> furtherIndex |> solveProgram
    | Mul mul -> applyMull mul state |> furtherIndex |> solveProgram
    | Exit    -> state

let parseProgram (input : string) =
    input.Split ','
    |> Array.mapi (fun i v -> (i,(int)v))
    |> Map.ofArray

let createSolverInput program = {Memory = program; InstructionPointer=0}
let parseSolverInput = parseProgram >> createSolverInput
let buildNounVerbParams (noun, verb) =  Map.add 1 noun >> Map.add 2 verb
let getProgramResult (state:State) = state.Memory.[0]

let solveWithParameters input parametrization = 
    input |>
    (parseProgram 
    >> parametrization 
    >> createSolverInput 
    >> solveProgram
    >> getProgramResult)


let solveForUserInput = solveWithParameters input

// for examples
let printableProgram {Memory=p} =
    p |> Map.toArray |> Array.map snd |> Array.fold (fun s value-> sprintf "%s%i;"s value ) ""
let solve = parseSolverInput >> solveProgram
let solveForString s = printfn "Result for [%s] %s" s (s |> solve |> printableProgram)

// part one specific
let partOneParametrization = buildNounVerbParams (12, 2)

// part two specific

let getPartTwoAnsertFromParameters (noun, verb) =
    100 * noun + verb

let parameterRange = [0..99]

let possibleInputs = 
    (parameterRange, parameterRange)
    |> List.cartesian

let szenarios =
    possibleInputs
    |> List.map (fun param -> (getPartTwoAnsertFromParameters param, param |> buildNounVerbParams |> solveForUserInput))

let matchPartTwo =
    szenarios
    |> List.filter (fun (code, output) -> output = 19690720)

[<EntryPoint>]
let main argv =
    solveForString "1,0,0,0,99"
    solveForString "2,3,0,3,99"
    solveForString "2,4,4,5,99,0"
    solveForString "1,1,1,4,99,5,6,0,99"
    printfn "Part One Result(should be 3790645): %i" (solveForUserInput partOneParametrization)
    printfn "Part Two Result(should be [(6577, 19690720)]): %A" matchPartTwo
    0 // return an integer exit code
