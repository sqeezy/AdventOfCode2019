// Learn more about F# at http://fsharp.org

open System


let input = @"147383
111288
130868
140148
79840
63305
98475
66403
68753
136306
94135
51317
136151
71724
68795
68526
130515
73606
56828
57778
86134
105030
123367
97633
85043
110888
110785
90662
128865
70997
90658
79944
141089
67543
78358
143579
146971
78795
94097
82473
73216
50919
100248
112751
86227
117399
123833
148570
141464
123266
94346
53871
51180
112900
119863
106694
129841
75990
63509
50135
140081
138387
112697
57023
114256
81429
95573
57056
52277
75137
53364
125823
113227
93993
129808
114025
101677
127114
65823
65834
57955
102314
60656
89982
61068
72089
71745
72460
142318
91951
111759
61177
143739
92202
70168
80164
77867
64235
141137
102636"

let partOneExampleInput1 = [|14|]
let partOneExampleInput2 = [|1969|]
let partOneExampleInput3 = [|100756|]

let partTwoExampleInput1 = [|14|]
let partTwoExampleInput2 = [|1969|]
let partTwoExampleInput3 = [|100756|]

let splitAtLinebreak (s : string) =
    s.Split([|'\n'|])

let fuelOfMass = (fun m -> m/3) >> (fun x -> x - 2)

let integerInput =
    input 
    |> splitAtLinebreak 
    |> Array.map (int) 


let partOneSolve = 
            Array.map fuelOfMass
            >> Seq.sum

let rec fuelForModul currentFuel remainingMass =
    let fuelForRemainingMass = fuelOfMass remainingMass
    if fuelForRemainingMass <= 0
    then currentFuel
    else fuelForModul (currentFuel+fuelForRemainingMass) fuelForRemainingMass

let partTwoSolve = 
            Array.map (fun m -> fuelForModul 0 m)
            >> Seq.sum

[<EntryPoint>]
let main _ =
    printfn "Result Part One DemoInput(should be %i): %i" (2) (partOneExampleInput1 |> partOneSolve)
    printfn "Result Part One DemoInput(should be %i): %i" (654) (partOneExampleInput2 |> partOneSolve)
    printfn "Result Part One DemoInput(should be %i): %i" (33583) (partOneExampleInput3 |> partOneSolve)
    printfn "Result Part One UserInput(should be 3160932): %i" (integerInput |> partOneSolve)

    printfn "Result Part Two DemoInput(should be %i): %i" (2) (partTwoExampleInput1 |> partTwoSolve)
    printfn "Result Part Two DemoInput(should be %i): %i" (966) (partTwoExampleInput2 |> partTwoSolve)
    printfn "Result Part Two DemoInput(should be %i): %i" (50346) (partTwoExampleInput3 |> partTwoSolve)
    printfn "Result Part Two UserInput(should be 4738549): %i" (integerInput |> partTwoSolve)
    0 // return an integer exit code
