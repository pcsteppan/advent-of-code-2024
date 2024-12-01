app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import "data.txt" as data : Str

lineSplitData = Str.splitOn data "\r\n"
numberPairData = List.map lineSplitData splitAndNumify
twoSortedLists = twoListsFromListOfPairs numberPairData
solution = absDiffOfTwoLists twoSortedLists
# differences = List.map numberPairData pairAbsDifference
# sum = List.sum differences


main =
    dbg { text: "TEST", value: 2 }
    dbg numberPairData
    dbg twoSortedLists
    dbg solution

    # dbg differences

    Stdout.line! (Num.toStr solution)

splitAndNumify = \str -> 
    pair = List.map (Str.splitOn str "   ") toI32
    (Result.withDefault (List.get pair 0) 0, Result.withDefault (List.get pair 1) 0)

toI32 = \str ->
    dbg str
    Str.toI32 str
    |> Result.withDefault 0

absDiffOfTwoLists = \twoLists ->
    absDiffs = List.map2 twoLists.0 twoLists.1 \a, b -> Num.abs (a - b)
    dbg absDiffs
    List.sum absDiffs

twoListsFromListOfPairs = \listOfPairs ->
    list1 = List.map listOfPairs \pair -> pair.0
    sortedList1 = List.sortAsc list1
    list2 = List.map listOfPairs \pair -> pair.1
    sortedList2 = List.sortAsc list2
    dbg sortedList1
    dbg sortedList2
    (sortedList1, sortedList2)