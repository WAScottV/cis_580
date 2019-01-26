open Microsoft.FSharp.Collections
open System

let rec max list =
    if List.length list = 1 then
        List.head list
    else
        if List.head list > max(List.tail list) then
            List.head list
        else
            max (List.tail list)

let rec nth n list =
    if n = 0 then
        List.head list
    else
        nth (n - 1) (List.tail list)

let rec zip (a, b) =
    if List.length a = 1 then
        [(List.head (a), List.head (b))]
    else
        [(List.head (a), List.head (b))] @ zip (List.tail (a), List.tail (b))

let rec search f list = 
    if (List.isEmpty list) then
        -1
    else if f (List.head (list)) then
        0
    else
        let result = search f (List.tail (list))
        if result = - 1 then
            -1
        else
            result + 1
let rec f n x =   
   if n = 0 then x
   else f (n - 1) (List.tail x)