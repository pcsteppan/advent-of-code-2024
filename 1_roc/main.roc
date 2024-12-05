app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.Stderr
import "data.txt" as data : Str

main =
    # part 1 - sum of absolute differences of sorted lists
    lineSplitData = Str.splitOn data "\r\n"
    numberPairData = List.map lineSplitData splitAndNumify
    twoSortedLists = twoListsFromListOfPairs numberPairData
    listA = twoSortedLists.0
    listB = twoSortedLists.1
    solution = absDiffOfTwoLists listA listB
        |> Num.toStr

    Stdout.line! "solution part 1: $(solution)"


    # part 2 - 'similarity score'
    frequenciesOfList1 = buildFrequencyDict listA
    frequenciesOfList2 = buildFrequencyDict listB

    solution2 = multiplyAndThenSumFrequencyDicts frequenciesOfList1 frequenciesOfList2
        |> Num.toStr
        
    Stdout.line! "solution part 2: $(solution2)"

splitAndNumify = \str -> 
    pair = List.map (Str.splitOn str "   ") toI32
    (Result.withDefault (List.get pair 0) 0, Result.withDefault (List.get pair 1) 0)

toI32 = \str ->
    Str.toI32 str
    |> Result.withDefault 0

absDiffOfTwoLists = \listA, listB ->
    absDiffs = List.map2 listA listB \a, b -> Num.abs (a - b)
    List.sum absDiffs

twoListsFromListOfPairs = \listOfPairs ->
    list1 = List.map listOfPairs \pair -> pair.0
    sortedList1 = List.sortAsc list1
    list2 = List.map listOfPairs \pair -> pair.1
    sortedList2 = List.sortAsc list2

    (sortedList1, sortedList2)

buildFrequencyDict = \list ->
    frequencyDict = List.walk list (Dict.empty {}) \dict, item ->
        Dict.update dict item insertOrIncrement

    insertOrIncrement : Result U64 [Missing] -> Result U64 [Missing]
    insertOrIncrement = \valueTag ->
        when valueTag is
            Err Missing -> Ok 1
            Ok count -> Ok (count + 1)

    frequencyDict

multiplyAndThenSumFrequencyDicts = \dict1, dict2 ->
    Dict.walk dict1 0 \sum, key, value ->
        unsignedKey: U64
        unsignedKey = Num.intCast key
        sum + (Dict.get dict2 key |> Result.withDefault 0) * value * unsignedKey
        
