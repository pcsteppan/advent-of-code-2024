import day5
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn parse_test() {
  let input =
    "1|2
2|3
3|4
2|4

1,2,3,4
"

  let expected =
    Ok(
      day5.Problem(
        rules: dict.from_list([#(2, [1]), #(3, [2]), #(4, [2, 3])]),
        candidates: [[1, 2, 3, 4]],
      ),
    )

  day5.parse(input)
  |> should.equal(expected)
}

pub fn solve_test() {
  let problem =
    day5.Problem(
      rules: dict.from_list([#(2, [1]), #(3, [2]), #(4, [2, 3])]),
      candidates: [[1, 2, 3, 3, 4], [4, 3, 2, 1]],
    )

  day5.solve(problem)
  |> should.equal(3)
}

fn real() -> String {
  let filepath = "test/data.txt"
  simplifile.read(filepath)
  |> result.unwrap("")
  |> string.trim
}

fn sample() -> String {
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"
}

pub fn part1_sample_test() {
  let input = sample()

  let expected = 143

  day5.parse(input)
  |> result.try(fn(problem) {
    let result = day5.solve(problem)
    io.debug("result p1 sample:")
    io.debug(result)
    Ok(result)
  })
  |> should.equal(Ok(expected))
}

pub fn ordering_test() {
  let input = sample()
  let problem =
    day5.parse(input)
    |> result.unwrap(day5.Problem(dict.new(), []))

  let _ordering = day5.ordering(problem.rules)
  // io.debug("ordering")
  // io.debug(ordering)
}

pub fn part1_test() {
  let filepath = "test/data.txt"
  let data =
    simplifile.read(filepath)
    |> result.unwrap("")
    |> string.trim

  let solution = day5.parse(data)
  case solution {
    Ok(problem) -> {
      let result = day5.solve(problem)
      io.debug("result p1:")
      io.debug(result)
      result
    }
    Error(e) -> {
      io.debug("error")
      io.debug(e)
      0
    }
  }
}

pub fn reorder_test() {
  let unordered = [1, 3, 2, 5]
  let ordering = [1, 2, 3, 4, 5]
  let reordered = day5.reorder(unordered, ordering)
  reordered |> should.equal([1, 2, 3, 5])
}

pub fn filter_rules_test() {
  let input = real()
  let problem =
    day5.parse(input)
    |> result.unwrap(day5.Problem(dict.new(), []))

  let real_candidate = [
    28, 91, 55, 75, 81, 23, 58, 33, 59, 36, 29, 62, 83, 98, 65, 11, 63, 94, 49,
    34, 26, 15, 69,
  ]

  day5.filter_rules_by_candidate(problem.rules, real_candidate)
  |> dict.to_list
  |> list.sort(fn(a, b) {
    let #(_k, v) = a
    let #(_k2, v2) = b
    int.compare(list.length(v), list.length(v2))
  })
  // |> list.map(io.debug(_))
}

pub fn reorder_2_test() {
  let input = real()
  let problem =
    day5.parse(input)
    |> result.unwrap(day5.Problem(dict.new(), []))

  let valid_candidate = [14, 97, 99]
  let valid_check = day5.solve_candidate(valid_candidate, problem.rules)
  valid_check |> should.equal(True)

  let filtered_rules =
    day5.filter_rules_by_candidate(problem.rules, valid_candidate)

  let ordered_rules =
    filtered_rules
    |> dict.to_list
    |> list.sort(fn(a, b) {
      let #(k, _v) = a
      let #(k2, _v2) = b
      int.compare(k, k2)
    })
    |> list.map(fn(a) {
      let #(k, v) = a
      #(k, list.sort(v, int.compare))
    })
    |> list.map(io.debug(_))
  // io.debug(ordered_rules)
}

pub fn part2_sample_test() {
  let input = sample()

  let solution = day5.parse(input)
  case solution {
    Ok(problem) -> {
      let result = day5.solve_part2(problem)
      io.debug("result p2 sample:")
      io.debug(result)
      result
    }
    Error(e) -> {
      io.debug("error")
      io.debug(e)
      0
    }
  }
}

pub fn part2_test() {
  let filepath = "test/data.txt"
  let data =
    simplifile.read(filepath)
    |> result.unwrap("")
    |> string.trim

  let input = data

  let solution = day5.parse(input)
  case solution {
    Ok(problem) -> {
      let result = day5.solve_part2(problem)
      io.debug("result p2:")
      io.debug(result)
      result
    }
    Error(e) -> {
      io.debug("error")
      io.debug(e)
      0
    }
  }
}
