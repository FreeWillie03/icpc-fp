module ICPC
open System
open System.Threading
open System.Threading
open System.Net
open System
open System.Runtime.InteropServices.ComTypes

(*Comma Sprinkler*)

//Get comma words 

let findAllWordsBefore word inputOption = 
    let rec loop word inputList wordBefore outputList=
        match inputList with 
        |[] -> (outputList)
        |head::tail -> match head = word with 
                       |true -> match wordBefore with 
                                |"" -> loop word tail wordBefore outputList
                                |_ -> loop word tail wordBefore (outputList @ [wordBefore])
                       |_ -> match head with 
                             |" "|"."|","|"" -> loop word tail wordBefore (outputList) 
                             |_ -> loop word tail head outputList
    match inputOption with 
    |Some x -> loop word x "" []
    |_ -> []

let getsucceededWords inputOption=
    let rec loop stringList previousWord outputList =
        match stringList with 
        |[] -> (outputList |> List.distinct)
        |head::tail-> 
            (*Finds all the comma after the word words*)
                      match head with 
                      |" "|"." -> loop tail previousWord outputList
                      |"," -> loop tail head (outputList @ [previousWord]) 
                      |_ -> match previousWord with 
                            |"" -> loop tail head outputList
                            |"," -> loop tail head (outputList @ (findAllWordsBefore head inputOption))
                            |_ -> loop tail head outputList
    match inputOption with 
    |Some x -> loop x "" []
    |None -> failwith ""


//End of get Comma words (takes in a string list option and returns string option list of the words that come before a comma)

let find word stringList =
    let rec loop word stringList previous= 
        match stringList with 
        |[] -> None
        |head::tail -> match previous = word with
                       |true -> match head with 
                                |","|"." -> loop word tail head
                                |_ -> Some word
                       |_ -> loop word tail head
    match stringList with 
    |[] -> None
    |_ -> loop word stringList ""

//Checks
let checkFirst inputList =
    match List.head(inputList) with 
    |" "|","|"." -> false
    |_ -> true
    
let checkOutput previousWords currentWords = 
    previousWords = currentWords 

let checkLength inputstring =
    match (inputstring |> String.length) > 1  with
    |true -> Some inputstring
    |_ -> None

let checkCommas input = 
   let rec loop inputList succededWords output = 
        match inputList with 
        |[] -> output
        |head::tail ->  match find head succededWords with 
                        |Some x -> true
                        |_ -> loop tail succededWords output
   match input with 
   |[] -> false             (*(Indicator of more commas to be added),(Some list)*)
   |_ -> loop input (getsucceededWords (Some input)) false

let checkCapitals (inputString:string) =
    inputString = inputString.ToUpper()

let checkSpaces (inputString:string) = 
    let rec loop inputstring previous =
        match inputstring with 
        |[] -> true
        |head::tail -> match previous with 
                       |' ' -> match head with 
                               |' '|'.' -> false
                               |_ -> loop tail head
                       |'.' -> match head with 
                               |' ' -> loop tail head
                               |_ -> false
                       |',' -> match head with 
                               |' ' -> loop tail head
                               |_ -> false
                       |_ -> loop tail head
    match  inputString |> checkLength with 
    |Some x -> loop (Seq.toList(x)) ' '
    |None -> false
//End of Checks


// String to List
let stringToStringList input = 
    let rec loop stringList word outputList =  
        match stringList with
        |[] -> match word with 
               |"." -> Some (outputList @ [word])  
               |_ -> None                
        |head::tail -> match head with 
                             |"?" -> None
                             |" " -> loop tail "" (outputList @ [word; head])
                             |","|"." -> loop tail head (outputList @ [word])
                             |_ -> match checkCapitals head with 
                                   |true -> None 
                                   |false -> loop tail (word + head) outputList
    match input |> checkLength with 
    |Some x -> loop (x |> Seq.toList |> List.map(fun (x:char) -> string x)) "" []
    |_ -> None
//End of String To List (takes in a stringOption and returns Some stringList if it ends in a "." and if the length is long enough) 



let FindWord inputWord inputList =
    let rec loop word inputList = 
        match inputList with 
        |[] -> false
        |head::tail -> match head = word with 
                       |true -> true
                       |false -> loop word tail
    match inputList with 
    |[] -> false
    |_ -> loop inputWord inputList

let populate stringList =
    let rec loop inputList previousWord commaList outputList =
        match inputList with 
        |[] -> Some (outputList @ [previousWord]),commaList 
        |head::tail -> match previousWord with 
                       |"" -> loop tail head commaList outputList
                       |_ -> match FindWord previousWord commaList with 
                             |true -> match head with (*word needs a comma So check the following word for ",\.\?"*)
                                      |","|"." -> loop tail head commaList (outputList @ [previousWord])
                                      |_ -> loop tail head commaList (outputList @ [previousWord;","])
                             |false -> loop tail head commaList (outputList @ [previousWord]) (*word doesnt need a comma*)
    match stringList with 
    |[] -> None,[]
    |_ -> loop stringList "" (getsucceededWords (Some stringList)) []



let ListToString inputList =
    let rec loop input output =
        match input with
        |[] -> output
        |head::tail -> match head with 
                       |"" -> loop tail output 
                       |_ -> loop tail (output+head)
    match inputList with 
    |[] -> ""
    |_ -> loop inputList ""

let commaSprinkler input =
    let rec loop inputList =
        match inputList with
        |[] -> None
        |_ -> match populate inputList with 
              |Some x, previous -> match (checkOutput previous (getsucceededWords (Some x))),(checkFirst x) with 
                                           |false,true -> loop x
                                           |true,true -> Some (ListToString x)
                                           |_,false -> None
                                           
              |None, _ -> None
    match checkSpaces input with 
    |true -> match input |> stringToStringList with 
             |Some x -> loop x 
             |None -> None
    |_ -> None

    (*End of Comma Sprinkler*)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//River doesnt work.
let checkPunctuation input =
    let rec loop inputstring =
        match inputstring with 
        |[] -> true
        |head::tail -> match head with 
                       |','|'.'|'!' -> false
                       |_ -> loop tail
    match input with
    |"" -> false
    |_ -> loop (Seq.toList(input))

let checkSingleSpace input =
    let rec loop input previous = 
        match input with 
        |[] -> match previous with 
               |' ' -> false
               |_ -> true
        |head::tail -> match head with 
                       |' ' -> match previous with
                               |' '|'?' -> false
                               |_ -> loop tail head
                       |_ -> loop tail head
    match input with
    |"" -> false
    |_ -> loop (Seq.toList(input)) '?'

let toListOfInt (input:string) = 
    let rec loop input word outputList=
        match input with 
        |[] -> (outputList @ [word])
        |head::tail -> match head with 
                       |' ' -> loop tail 0 (outputList @ [word; 0])
                       |_ -> loop tail (word+1) outputList
    loop (Seq.toList(input)) 0 []

let containsSpace (input:string) =
    input.Contains(" ")

let checkWordLength inputList =
    let rec loop input =
            match input with
            |[] -> true
            |head::tail -> match head < 81 with 
                           |true -> loop tail
                           |false -> false
    match inputList with
    |[] -> false
    |_ -> loop inputList

let getListlength input =
    match input with 
    |[] -> 0
    |_ -> List.sumBy(fun x -> match x = 0 with |true -> 1 |_ -> x) <| input

let getLongest input = 
    let rec loop input longest = 
        match input with 
        |[] -> longest
        |head::tail -> match head > longest with 
                 |true -> loop tail head
                 |_ -> loop tail longest
    match input with 
    |[] -> 0
    |_ -> loop input 0

let checkValidWidth input width = //false is stoping case for the width decramenting
    getLongest input < width
//List.count head and head
(*
let getRiver input width =
    let rec loop input width =
        match checkValidWidth (width) with 
        |true -> []
        |_ -> []
    match input with 
    |[] -> []
    |_ -> loop input width
*)
let posOfSpace input =
    let rec loop input pos output =
        match input with 
        |[] -> output
        |head::tail -> match head=0 with 
                       |true -> match pos%15 = 0 with 
                                |true -> loop tail (pos+1) (output)
                                |_ -> loop tail (pos+1) (output @ [pos%15])
                       |_ -> loop tail (pos+head) output
    match input with 
    |[] -> []
    |_ -> loop input 0 []

 
let validString (inputString:string) =
    match (checkPunctuation inputString),(checkSingleSpace inputString),((toListOfInt inputString) |> checkWordLength),(containsSpace inputString) with
    |true,true,true,true -> match (posOfSpace (inputString |> toListOfInt) |> List.length) = 1 with 
                            |true -> Some (getListlength (inputString |> toListOfInt) , 1)
                            |_-> None //for the other two cases
    |_ -> None

let rivers (input:string) =
    validString input

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

    (*string before after*)
