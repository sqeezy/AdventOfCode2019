// Learn more about F# at http://fsharp.org

open System

type Instructions = (int*int*int) 

type Operation = 
    | Add of Instructions
    | Mul of Instructions
    | Exit

type State = {
    Program : Map<int, int>
    NextIndex : int
}

let getInstructions {Program=p;NextIndex=i} =
    let a = p.[i+1]
    let b = p.[i+2]
    let t = p.[1+3]
    (a, b, t)

let parseOperation state =
    let {Program=p;NextIndex=i} = state
    match p.[i] with
    | 1     -> Add <| getInstructions state
    | 2     -> Mul <| getInstructions state
    | 99    -> Exit
    | _     -> raise (ArgumentException "foo")

let applyAdd add  state =
    let {Program=p;NextIndex=i} = state
    let (a,b,t) = add
    let sum = a + b
    let updatedMap = Map.add t sum p
    {state with Program = updatedMap};

let applyMull add  state =
    let {Program=p;NextIndex=i} = state
    let (a,b,t) = add
    let sum = a * b
    let updatedMap = Map.add t sum p
    {state with Program = updatedMap};

let furtherIndex state = {state with NextIndex = state.NextIndex + 4}

let rec solveProgram state =
    match parseOperation state with
    | Add add -> applyAdd add state |> furtherIndex |> solveProgram
    | Mul mul -> applyMull mul state |> furtherIndex |> solveProgram
    | Exit    -> state


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
