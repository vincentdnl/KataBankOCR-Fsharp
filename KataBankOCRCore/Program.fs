module KataBankOCRCore

type DigitAsStrings = string * string * string

let zero : DigitAsStrings = (" _ ", "| |", "|_|")
let one : DigitAsStrings = ("   ", "  |", "  |")
let two : DigitAsStrings = (" _ ", " _|", "|_ ")
let three : DigitAsStrings = (" _ ", " _|", " _|")
let four : DigitAsStrings = ("   ", "|_|", "  |")
let five : DigitAsStrings = (" _ ", "|_ ", " _|")
let six : DigitAsStrings = (" _ ", "|_ ", "|_|")
let seven : DigitAsStrings = (" _ ", "  |", "  |")
let eight : DigitAsStrings = (" _ ", "|_|", "|_|")
let nine : DigitAsStrings = (" _ ", "|_|", " _|")

let ones = "                           \n  |  |  |  |  |  |  |  |  |\n  |  |  |  |  |  |  |  |  |\n"

let concatDigits (digit: DigitAsStrings) (digitList: string * string * string) : string * string * string =
    let d1, d2, d3 = digit
    let l1, l2, l3 = digitList
    d1+l1, d2+l2, d3+l3

let digitToString (digit : DigitAsStrings) : string =
    let line1, line2, line3 = digit
    line1+"\n"+line2+"\n"+line3

let printDigit (digit : DigitAsStrings) : unit =
    let line1, line2, line3 = digit
    printfn "%s\n%s\n%s" line1 line2 line3

let charListToString (chars: char list) : string =
    let rec workerFunc (charList : char list) (str: string) =
        match charList with
        | [] -> str
        | head::tail -> workerFunc tail (str + (string head))
    workerFunc chars ""

let stringSplitter (s: string) : (char list * char list * char list) =
    let rec extractLine (text: char list) (accu: char list) : (char list * char list) =
        match text with
        | [] -> (accu, [])
        |'\n'::tail -> (accu, tail)
        | head::tail -> extractLine tail (accu @ [head])

    let text = Seq.toList s

    let rec extractLines (text: char list) (lines: char list list) : char list list =
        match text with
        | [] -> lines
        | _ ->
            let line, rest = extractLine text []
            extractLines rest (lines @ [line])
            
    match extractLines text [] with
    | (l1:char list)::(l2:char list)::[(l3:char list)] -> (l1,l2,l3)
    | _ -> failwith "wrong number of lines in input"


let extractCharacter (lines: char list * char list * char list) : (DigitAsStrings * (char list * char list * char list)) =
    match lines with
    | a1::a2::a3::taila, b1::b2::b3::tailb, c1::c2::c3::tailc -> ((charListToString (a1::a2::[a3])), (charListToString (b1::b2::[b3])), (charListToString (c1::c2::[c3]))), (taila, tailb, tailc)
    | _ -> failwith "incorrect char list triplet found"

let tokenize (lines: char list * char list * char list) : DigitAsStrings list =
    let rec workerFunc (lines: char list * char list * char list) (accu: DigitAsStrings list) =
        match lines with
        | [], [], [] -> accu
        | head0::tail0, head1::tail1, head2::tail2 ->
            let token, rest = extractCharacter lines
            workerFunc rest (accu @ [token])
        | _ -> failwith "incorrect char list triplet found"
    workerFunc lines []

let parse (digit: DigitAsStrings) : string =
    match digit with
    | x when x = zero -> "0"
    | x when x = one -> "1"
    | x when x = two -> "2"
    | x when x = three -> "3"
    | x when x = four -> "4"
    | x when x = five -> "5"
    | x when x = six -> "6"
    | x when x = seven -> "7"
    | x when x = eight -> "8"
    | x when x = nine -> "9"
    | _ ->
        let d1,d2,d3 = digit
        failwith (sprintf "unknown digit token encountered %s,%s,%s" d1 d3 d3)

let parseAll (digits: DigitAsStrings list) : string =
    let rec workerFunc (digits: DigitAsStrings list) (accu: string) : string =
        match digits with
        | [] -> accu
        | head::tail -> workerFunc tail (accu + (parse head))
    workerFunc digits ""

let getAccountNumber (accountNumberOcr: string) = parseAll (tokenize (stringSplitter accountNumberOcr))



let test1 =
    let twoDigits = concatDigits one two
    let threeDigits = concatDigits five twoDigits
    let fourDigits = concatDigits seven threeDigits
    let fiveDigits = concatDigits two fourDigits
    let sixDigits = concatDigits eight fiveDigits
    let sevenDigits = concatDigits six sixDigits
    let eightDigits = concatDigits zero sevenDigits
    let nineDigits = concatDigits four eightDigits

    getAccountNumber (digitToString nineDigits)

printf "%s" test1;;
