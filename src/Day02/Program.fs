// Learn more about F# at http://fsharp.org

open System

type Instructions = (int*int*int) 

type Operation = 
    | Add of Instructions
    | Mul of Instructions
    | Exit

type Program = int[]

type State = {
    Program : Program
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
    | 1     -> Add <| getInstructions state , state
    | 2     -> Mul <| getInstructions state ,state
    | 99    -> Exit, state

let applyAdd add state =


let applyOperation (operation, state) =
    match operation with
    | Add (a,b,t), state -> 0 
    | Mul (a,b,t), state -> 0
    | Exit       , state -> 0

let rec solveProgram state =
    match parseOperation state with
    | Add (a,b,t), state -> 0 
    | Mul (a,b,t), state -> 0
    | Exit       , state -> 0


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
