// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

    let primes = [3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151; 157; 163; 167; 173; 179; 181; 191; 193; 197; 199]
    let list = seq{25 .. 25000} |> Seq.toList 
    let rnd = System.Random(50)

    // a^b % c
    let superiorMod (baseNum: float) (power: int) (modulo: float) =
        let rec modInner (power: int) (result : float) =
            match power with
            | 0 -> result
            | _ -> modInner (power - 1) (result * baseNum % modulo)
        modInner power 1.0

    let del1 m s = 
        let rec inner s t =
            match t with
            | t when t % 2.0 <> 0.0 -> (s, t)
            | _   -> inner (s+1.0) ((m-1.0)/(2.0**(s+1.0)))
        inner 0.0 ( (m-1.0)/(2.0**s) )

    let rec loop y m s =
        match s with 
        | -1.0 -> false
        | _ -> if ( ((-1.0 - y) % m) = 0.0) then true else loop (superiorMod y 2 m) m (s-1.0)

    let rec del2 m k =
        match k with
        | 0 -> true
        | _ -> 
            if m % 2.0 = 0.0 then false else
            let b      = rnd.Next(1,int m)
            let (s, t) = del1 m 0.0
            let y      = superiorMod (float b) (int t) m
            let res    = if ( ((1.0 - y) % m) = 0.0) then true else loop y m (s-1.0)
            if res then del2 m (k-1) else false

    let trialDivision m =
        let rlist = List.filter (fun x -> float x <= sqrt (float m)) primes
        let rec inner m' rlist' =
            match rlist' with
            | []    -> true
            | r::rx -> if m' % r = 0 then false else inner m' rx 
        if m % 2 = 0 then false else inner m rlist
        

    // Trial Divison test between 25-25000
    let (p, np) =   
        List.fold (fun (p, np) m ->
            if trialDivision m then (p+1, np) else (p, np+1)  
        ) (0,0) list

    // Miller-Rabin test between 25-25000
    let (p', np') =
        List.fold (fun (p, np) m ->
            if del2 (float m) 3 then (p+1, np) else (p, np+1)  
        ) (0,0) list

    printfn("Trial Division test: %A") (p, np)
    printfn("Miller-Rabin test: %A") (p', np')
  
    0 // return an integer exit code
