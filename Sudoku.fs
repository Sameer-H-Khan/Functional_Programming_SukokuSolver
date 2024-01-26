// For more information see https://aka.ms/fsharp-console-apps
open System

// module Solver = 
type Board = int [,]

////////////// Helpers for determining validity of board //////////////
let setValue (board: Board, row: int, col: int, value: int) =
    // let newBoard = Array2D.copy board
    // newBoard.[row, col] <- value
    // newBoard
    let newBoard = Array2D.init 9 9 (
        fun i j -> 
            if i = row && j = col then value else board.[i, j]
    )
    newBoard 

let getRow(board : Board, idx : int) = 
    let numCols = Array2D.length2 board
    [| for col in 0 .. numCols - 1 -> board.[idx, col] |]

let getCol(board : Board, idx : int) = 
    let numRows = Array2D.length2 board
    [| for row in 0 .. numRows - 1 -> board.[row, idx] |]

let getSquare(board : Board, row : int, col : int) = 
    let rowStart = (row / 3) * 3
    let colStart = (col / 3) * 3

    // We use the yield keyword to add each value of 3x3 square to returned sequence
    [| for i in 0..2 do
        for j in 0..2 do
            yield board.[rowStart + i, colStart + j] |]    

let hasDuplicates (lst) = 
    lst
        |> Array.filter (fun x -> x <> 0) // filters out zeros
        |> Array.groupBy (fun x -> x)     // [1;2;3;1] -> [|(1, [|1; 1|]); (2, [|2|]); (3, [|3|]);
        |> Array.exists (fun (_, values) -> values.Length > 1) // is there one of those arrays with length over 1

// Determines if a placement of a number at row, col is valid
let isValid(board : Board, row : int, col : int) : Boolean = 
    let currRow = getRow(board, row)
    let currCol = getCol(board, col)
    let currSqr = getSquare(board, row, col)
    not (hasDuplicates(currRow) || hasDuplicates(currCol) || hasDuplicates(currSqr))

// Determines if a board will be valid after placing value at row, col
let isValidPlacement(oldBoard : Board, row : int, col : int, value : int) : Boolean = 
    let board = setValue(oldBoard, row, col, value)
    let currRow = getRow(board, row)
    let currCol = getCol(board, col)
    let currSqr = getSquare(board, row, col)
    not (hasDuplicates(currRow) || hasDuplicates(currCol) || hasDuplicates(currSqr))

// Determines validity of an entire board
let isValidBoard(board : Board) : Boolean =
    let numRows = Array2D.length1 board
    let numCols = Array2D.length2 board

    let rec isValidRec row col = 
        if row < numRows then
            if col < numCols then
                if not (isValid(board, row, col)) then
                    false
                else
                    isValidRec row (col + 1)
            else
                isValidRec (row + 1) 0
        else
            true

    isValidRec 0 0 


////////////// Solving board //////////////

// Finds the first occurrence of an empty cell (0) in a board 
// I ended up not using this
let findZeroIndices (matrix: Board) = 
    let numRows = Array2D.length1 matrix
    let numCols = Array2D.length2 matrix

    let rec findZero row col =
        if row < numRows then
            if col < numCols then
                if matrix.[row, col] = 0 then
                    Some (row, col)
                else
                    findZero row (col + 1)
            else
                findZero (row + 1) 0
        else
            None

    findZero 0 0

// Uses backtracking to find a valid solution. Returns None if a branch in the recursion
// tree has no solution, returns a Board if there's a valid solution
let rec solve(board : Board, x : int, y : int) = 
    if y >= 9 && isValidBoard(board) then 
        Some board
    else if y >= 9 then 
        None
    else if x >= 9 then 
        solve(board, 0, y + 1)
    else if board.[x, y] > 0 then 
        solve(board, x + 1, y)
    else 
        let validNums = List.filter (fun num -> isValidPlacement(board, x, y, num)) [1..9]
        let tryValidNum num = 
            let newBoard = setValue(board, x, y, num)
            solve(newBoard, x + 1, y)
        validNums |> Seq.tryPick tryValidNum    // Applies tryValidNum to elements in validNums, 
                                                //  returning first value where the function returns Some


//////////////////////////////////////////////////////////////////////////
////////////// Tests and helpers for testing Sudoku solving //////////////
let makeSudokuBoard(board : int array array) : Board = 
    Array2D.copy (Array2D.init 9 9 (fun row col -> board.[row].[col]))

let b = hasDuplicates([|1;2;3;4;0;0;0;0;0;5;6;7|])
printfn "Does %A have non-zero duplicates: %A" [|1;2;3;4;0;0;0;0;0;5;6;7|] b

let arrr = [|
                [|6; 0; 0;   0; 7; 5;   0; 0; 9|];
                [|7; 8; 0;   4; 0; 0;   1; 2; 0|];
                [|0; 0; 0;   6; 0; 1;   0; 7; 8|];

                [|0; 0; 7;   0; 4; 0;   2; 6; 0|];
                [|0; 0; 1;   0; 5; 0;   9; 3; 0|];
                [|9; 0; 4;   0; 6; 0;   0; 0; 5|];
                
                [|0; 7; 0;   3; 0; 0;   0; 1; 2|];
                [|1; 2; 0;   0; 0; 7;   4; 0; 0|];
                [|0; 4; 9;   2; 0; 6;   0; 0; 7|]
            |]
let dummy = makeSudokuBoard(arrr)
printfn "\nGetting rows, columns, and squares from \n%A" dummy
let row = getRow(dummy, 1)
printfn "Row 1 %A" row

let col = getCol(dummy, 4)
printfn "Col 4 %A" col

let sqr = getSquare(dummy, 5, 1)
printfn "Square at 5, 1: %A" sqr

let result = findZeroIndices dummy
match result with
| Some (row, col) -> printfn "First occurrence of 0 at (%d, %d)" row col
| None -> printfn "0 not found in the matrix"

let stupid = [|[|1; 9; 9|]; [|9;0;9|]; [|0;9;9|]|]
let dummy2 = Array2D.copy (Array2D.init 3 3 (fun row col -> stupid.[row].[col]))
let result2 = findZeroIndices dummy2
match result2 with
| Some (row, col) -> printfn "First occurrence of 0 at (%d, %d)" row col
| None -> printfn "0 not found in the matrix"

printfn "\n\n############## Starting the board solving tests ##################"
printfn "Solving this board:\n %A" dummy
printfn "Solved: "
let resultFinal = solve(dummy, 0, 0)
printfn "%A" resultFinal


let dummy3 = 
    [|
        [|0; 7; 2;  0; 8; 0;  5; 3; 0|];
        [|0; 0; 0;  0; 2; 0;  0; 0; 0|];
        [|0; 0; 4;  1; 9; 7;  8; 0; 0|];

        [|1; 2; 8;  0; 4; 0;  6; 5; 7|];
        [|5; 0; 0;  7; 1; 8;  0; 0; 3|];
        [|0; 0; 0;  0; 0; 0;  0; 0; 0|];

        [|2; 3; 1;  8; 5; 9;  4; 7; 6|];
        [|0; 8; 5;  6; 7; 1;  3; 2; 0|];
        [|6; 9; 7;  2; 3; 4;  1; 8; 5|]
    |]
let dummy3Arr = makeSudokuBoard(dummy3)
printfn "\nSolving dummy3:\n %A" dummy3Arr
printfn "Solved: "
let result3 = solve(dummy3Arr, 0, 0)
printfn "%A" result3

let dummy4 = 
    [|
        [|0; 9; 0;   7; 0; 0;   8; 6; 0|];
        [|0; 3; 1;   0; 0; 5;   0; 2; 0|];
        [|8; 0; 6;   0; 0; 0;   0; 0; 0|];

        [|0; 0; 7;   0; 5; 0;   0; 0; 6|];
        [|0; 0; 0;   3; 0; 7;   0; 0; 0|];
        [|5; 0; 0;   0; 1; 0;   7; 0; 0|];

        [|0; 0; 0;   0; 0; 0;   1; 0; 9|];
        [|0; 2; 0;   6; 0; 0;   3; 5; 0|];
        [|0; 5; 4;   0; 0; 8;   0; 7; 0|]
    |]

let dummy4Arr = makeSudokuBoard(dummy4)
printfn "\nSolving dummy4:\n %A" dummy4Arr
printfn "Solved: "
let result4 = solve(dummy4Arr, 0, 0)
printfn "%A" result4

let dummy5 = 
    [|
        [|3; 8; 5;   0; 0; 0;   0; 0; 0|]; 
        [|9; 2; 1;   0; 0; 0;   0; 0; 0|]; 
        [|6; 4; 7;   0; 0; 0;   0; 0; 0|]; 

        [|0; 0; 0;   1; 2; 3;   0; 0; 0|]; 
        [|0; 0; 0;   7; 8; 4;   0; 0; 0|]; 
        [|0; 0; 0;   6; 9; 5;   0; 0; 0|]; 

        [|0; 0; 0;   0; 0; 0;   8; 7; 3|]; 
        [|0; 0; 0;   0; 0; 0;   9; 6; 2|]; 
        [|0; 0; 0;   0; 0; 0;   1; 4; 5|]
    |]
let dummy5Arr = makeSudokuBoard(dummy5)
printfn "\nSolving dummy5:\n%A" dummy5Arr
printfn "Solved: "
let result5 = solve(dummy5Arr, 0, 0)
printfn "%A" result5


///////////////////////////////////////////////////////////////////
////////////// Helpers for generating a sudoku board //////////////
let shuffleList (list: 'a list) =
    let rnd = System.Random()
    let shuffle (arr: 'a[]) =
        let swap i j =
            let tmp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp
        for i = arr.Length - 1 downto 1 do
            let j = rnd.Next(i + 1)
            swap i j
        arr
    list |> List.toArray |> shuffle |> Array.toList
    

let fillInDiagonalBoxes() = 
    let lst1 = shuffleList [1..9]
    let lst2 = shuffleList [1..9]
    let lst3 = shuffleList [1..9]

    let box1 = Array2D.init 3 3 (fun i j -> lst1.[i * 3 + j])
    let box2 = Array2D.init 3 3 (fun i j -> lst2.[i * 3 + j])
    let box3 = Array2D.init 3 3 (fun i j -> lst3.[i * 3 + j])

    let sudokuBoard = Array2D.init 9 9 (
        fun i j -> 
            if 0 <= i && i < 3 && 0 <= j && j < 3 then 
                box1.[i, j] 
            else if 3 <= i && i < 6 && 3 <= j && j < 6 then
                box2.[i - 3, j - 3]
            else if 6 <= i && i < 9 && 6 <= j && j < 9 then
                box3.[i - 6, j - 6]
            else 0
    )
    sudokuBoard 

let generateKRandomIndices(k, rows, cols) = 
    let rnd = new Random()

    let rec collectIndices(indicesLst) = 
        if List.length indicesLst >= k then indicesLst
        else 
            let index = (rnd.Next(0, rows), rnd.Next(0, cols))
            if List.contains index indicesLst then
                collectIndices(indicesLst)
            else collectIndices(index :: indicesLst)

    collectIndices([])

////////////// Generating a sudoku board //////////////
// We generate a fully-filled sudoku board by filling in the 3x3 boxes along the diagonal
// Then, solve the board. Finally, remove random elements from the solved board 
let generateSudokuBoard(numEmptyCells : int) = 
    let diagonalsBoard = fillInDiagonalBoxes()
    let solvedBoard = solve(diagonalsBoard, 0, 0)
    printfn "\nSolved board, now zeroing some elements: \n%A" solvedBoard
    let emptyIndices = generateKRandomIndices(numEmptyCells, 9, 9)
    match solvedBoard with
    // Take the solved board, apply a filter that sets elements to zero at the indices in emptyIndices
    | Some board -> board |> Array2D.mapi (fun i j x -> if List.exists (fun (a, b) -> a = i && b = j) emptyIndices then 0 else x)
    | None -> Array2D.zeroCreate 9 9


//////////////////////////////////////////////////////////////////
////////////// Testing sudoku generator and helpers //////////////
let sss = [1..9]
let sssf = shuffleList sss
printfn "Suffling %A: %A" sss sssf

let reshapedArr = Array2D.init 3 3 (fun i j -> sssf.[i * 3 + j])
printfn "Testing reshape array: %A" reshapedArr


let initBoard = fillInDiagonalBoxes()
printfn "%A" initBoard

let indices = generateKRandomIndices(9, 3, 3)
printfn "Indices: %A" indices


let generatedBoard = generateSudokuBoard 30
printfn "Generated board: \n%A" generatedBoard

printfn("Done !!!!")

