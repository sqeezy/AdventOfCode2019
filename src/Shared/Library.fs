namespace Shared

module String =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

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
