app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.Stderr

import "data.txt" as data : Str

sample =
    """7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9"""

main =
    lines = parse data
    dbg lines
    diffs = List.map lines getDiffs

    validLevels = List.map diffs isValid

    count = List.countIf validLevels \x -> x
    
    Stdout.line! (Num.toStr count)

    # runTests

    validLevelsWithDampening = List.map diffs removeFirstInvalidDifference
        |> List.map isValidWithDampening

    count2 = List.countIf validLevelsWithDampening \x -> x

    # 2 = 589

    expect count2 == 589

    Stdout.line! (Num.toStr count2)

    Stdout.line! "All tests passed"


parse = \str ->
    Str.splitOn str "\r\n"
        |> List.map parseLine

parseLine = \str ->
    Str.splitOn str " "
        |> List.map Str.toI32
        |> List.map \n -> Result.withDefault n 0

getDiffs = \list ->
    len = List.len list
    a = List.sublist list { start: 0, len: len - 1 }
    b = List.sublist list { start: 1, len }
    # dbg {a, b}
    r = List.map2 a b \x, y -> y - x
    # dbg r
    r

isValid = \list ->
    (List.all list \x -> x > 0 && x <= 3)
    || (List.all list \x -> x < 0 && x >= -3)

isValidWithDampening = \list ->
    hasExtremes = List.any list \x -> x < -3 || x > 3 || x == 0
    if hasExtremes then
        # dbg HasExtremes
        Bool.false
    else
        positiveCount = List.countIf list \x -> x > 0 && x <= 3
        result = 
            if positiveCount == List.len list then
                Bool.true
            else
                negativeCount = List.countIf list \x -> x < 0 && x >= -3
                negativeCount == List.len list
        # dbg {result, list}
        result

removeFirstInvalidDifference = \list ->
    # remove outlier at start or end
    # combine neighboring outliers in middle
    initialState = (Incomplete, 0, [])
    result = List.walkWithIndex list initialState \state, item, i ->
        (isCompleted, lastOutlier, newList) = state
        atEdge = i == 0 || i == List.len list - 1
        # dbg {atEdge, i, state}
        if isCompleted == Completed then
            updated = List.append newList item
            (Completed, 0, updated)
        else if lastOutlier != 0 then
            updated = List.append newList (lastOutlier + item)
            (Completed, 0, updated)
        else if item <= 3 && item >= -3 && item != 0 then
            updated = List.append newList item
            (isCompleted, 0, updated)
        else if atEdge then
            (Completed, 0, newList)
        else
            (Incomplete, item, newList)

    # dbg result

    result.2

runTests = \_ ->
    # [1, 2, 3, 4] -> [1, 1, 1] -> [1, 1, 1]
    expect getDiffs [1, 2, 3, 4]
        |> removeFirstInvalidDifference
        |> Bool.isEq [1, 1, 1]
    
    # [10, 2, 3, 4] -> [-8, 1, 1] -> [1, 1]
    expect getDiffs [10, 2, 3, 4]
        |> removeFirstInvalidDifference
        |> Bool.isEq [1, 1]

    # [1, 2, 3, 10] -> [1, 1, 7] -> [1, 1]
    expect getDiffs [1, 2, 3, 10]
        |> removeFirstInvalidDifference
        |> Bool.isEq [1, 1]

    # [1, 2, 10, 4] -> [1, 8, -6] -> [1, 2]
    expect getDiffs [1, 2, 10, 4]
        |> removeFirstInvalidDifference
        |> Bool.isEq [1, 2]

    # [-1, -2, -10, -10, -4] -> [-1, -8, 0, 6] -> [-1, -8, 6]
    expect getDiffs [-1, -2, -10, -10, -4]
        |> removeFirstInvalidDifference
        |> Bool.isEq [-1, -8, 6]

    # [2, 1, 1] -> [-1, 0] -> [-1]
    expect getDiffs [2, 1, 1]
        |> removeFirstInvalidDifference
        |> Bool.isEq [-1]

    {}