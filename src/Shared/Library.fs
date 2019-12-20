namespace Shared

module String =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None
    let split char (s : string) =
        s.Split [|char|]

    let splitMany chars (s : string) =
        s.Split chars

module List =
    let rec cartesian = function
     | ([],[]) -> []
     | (_ ,[]) -> []
     | ([], _) -> []
     | (x::xs, ys) -> (List.map(fun y -> x,y) ys) @ (cartesian (xs,ys))

module Log =
    let logWithLabel<'a> s a = 
        printfn "%s %A" s a
        a
