// Learn more about F# at http://fsharp.org

open Shared
open System
open Shared.String
open System.IO
open System.Text

let userInput = File.ReadAllText("./src/Day03/input.txt")

type Direction = Left | Up | Right | Down
let directionVectors = function
    | Left -> (-1, 0)
    | Up   -> (0, 1)
    | Right-> (1, 0)
    | Down -> (0, -1)

type WireSection = (Direction * int)
type Wire = {
    Sections : WireSection array
    Id : Guid
}
type Position = (int * int)
let (.*) ((x,y):Position) (scalar : int) = (x * scalar, y * scalar)
let (.+) (x1, y1) (x2, y2)  = (x1 + x2, y1 + y2)
type Field =
    | One of Guid
    | Two
type WireField = Map<Position, Field>
type WireApplicationState = (WireField * Position * Guid)
let start = (0, 0)

let applyPositionForGuid id (curField:WireField) position =
    match Map.tryFind position curField with
    | None -> Map.add position (One id) curField
    | Some field ->
        match field with
        | One currentId when currentId <> id ->
             Map.add position Two curField
        | One _  -> curField
        | Two -> raise (Exception "")

let wireFolder (field, currentPosition, id) (direction, sectionLength) =
    let newPositions = [1..sectionLength] |> List.map (fun i -> currentPosition .+ ((direction |> directionVectors) .* i))
    let applyPosition = applyPositionForGuid id
    let newField = List.fold applyPosition field newPositions
    let newCurrentPosition = newPositions |> List.rev |> List.head
    (newField, newCurrentPosition, id)

let applyWire field {Sections = sections; Id = id} =
    let initialApplicationState = (field, start, id)
    let (filledField, _ , _) = Array.fold wireFolder initialApplicationState sections
    filledField

let emptyField = Map.empty

let parseWireSection (s:string) =
    match s with
    | Prefix "L" rest -> (Left, rest |> int)
    | Prefix "U" rest -> (Up, rest |> int)
    | Prefix "R" rest -> (Right, rest |> int)
    | Prefix "D" rest -> (Down, rest |> int)
    | _               -> raise (Exception (sprintf "Input Invalid: %s" s))

let wire sections = 
    {Sections = sections; Id = Guid.NewGuid()}

let layoutWires =
    (split '\n')
    >> Array.map (split ',')
    >> Array.map (Array.map parseWireSection)
    >> Array.map wire
    >> Array.fold applyWire emptyField
    >> Map.filter (fun _ field -> field = Two)
    >> Map.toList
    >> List.map fst 
    >> List.sortBy (fun (x, y)-> Math.Abs x + Math.Abs y)
    >> List.head
    >> (fun (x, y) -> Math.Abs x + Math.Abs y)

let example1 = @"R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83";

let example2 = @"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

[<EntryPoint>]
let main _ =
    
    printfn "%A" (layoutWires example1)
    printfn "%A" (layoutWires example2)
    printfn "%A" (layoutWires userInput)
    0 // return an integer exit code
