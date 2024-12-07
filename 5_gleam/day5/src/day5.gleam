import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/result
import gleam/set
import gleam/string

pub fn main() {
  io.println("Hello from day5!")
}

pub type Rules =
  dict.Dict(Int, List(Int))

pub type Candidates =
  List(List(Int))

pub type Problem {
  Problem(rules: Rules, candidates: Candidates)
}

// return conut of valid candidates
pub fn solve(problem: Problem) -> Int {
  let middle_numbers_of_valid_candidates =
    list.filter(problem.candidates, fn(candidate) {
      let is_valid = solve_candidate(candidate, problem.rules)
      io.debug("is valid problem?")
      io.debug(is_valid)
      is_valid
    })
    |> list.map(get_middle_element)

  list.fold(middle_numbers_of_valid_candidates, 0, int.add)
}

pub fn solve_part2(problem: Problem) -> Int {
  let ordering = ordering(problem.rules)
  io.debug("ordering")
  io.debug(ordering)
  let sum =
    list.map(problem.candidates, fn(candidate) {
      case solve_candidate(candidate, problem.rules) {
        True -> candidate
        False -> reorder(candidate, ordering)
      }
    })
    |> list.map(get_middle_element)
    |> io.debug
    |> list.fold(0, int.add)

  io.debug("sum")
  io.debug(sum)
  // io.debug("reordered_candidates")
  // io.debug(reordered_candidates)

  // let total_sum = solve(Problem(problem.rules, reordered_candidates))
  // io.debug("total sum")
  // io.debug(total_sum)

  sum
}

pub fn ordering(rules: Rules) -> List(Int) {
  io.debug(dict.to_list(rules))

  let full_set =
    dict.to_list(rules)
    |> list.fold(set.new(), fn(set, kvp) {
      let #(_, value) = kvp
      list.fold(value, set, fn(set, num) { set.insert(set, num) })
    })

  set.insert(full_set, 0)

  let ordering: List(Int) =
    dict.to_list(rules)
    |> list.sort(fn(a, b) {
      case list.length(a.1) - list.length(b.1) {
        n if n < 0 -> order.Lt
        n if n == 0 -> order.Eq
        n if n > 0 -> order.Gt
        _ -> order.Eq
      }
    })
    |> list.map(fn(kvp) { kvp.0 })

  let t_set = set.from_list(ordering)
  let diff = set.difference(full_set, t_set)

  let diff_list = set.to_list(diff)

  list.append(diff_list, ordering)
}

pub fn reorder(unordered: List(Int), ordering: List(Int)) -> List(Int) {
  let set1 = set.from_list(unordered)
  list.filter(ordering, fn(num) { set.contains(set1, num) })
}

fn get_middle_element(list: List(Int)) -> Int {
  let upper_middle_count = list.length(list) / 2 + 1
  let upper_middle_part = list.take(list, upper_middle_count)
  let middle_element =
    list.last(upper_middle_part)
    |> result.unwrap(0)

  middle_element
}

fn solve_candidate(candidate: List(Int), rules: Rules) -> Bool {
  case candidate {
    [] | [_] -> True
    [head, ..tail] -> {
      let must_come_afters = dict.get(rules, head) |> result.unwrap([])
      io.debug(must_come_afters)
      case intersects(must_come_afters, tail) {
        True -> False
        False -> solve_candidate(tail, rules)
      }
    }
  }
}

fn intersects(list1: List(Int), list2: List(Int)) -> Bool {
  io.debug("intersects")
  io.debug(list1)
  io.debug(list2)
  io.debug("---")
  case list1 {
    [] -> False
    list -> {
      list.filter(list1, fn(num) { list.contains(list2, num) })
      |> list.length
      > 0
    }
  }
}

pub fn parse(input: String) -> Result(Problem, String) {
  let cleaned_input = input |> string.trim
  case string.split_once(cleaned_input, "\n\n") {
    Ok(#(rules, candidates)) -> {
      // io.debug(rules)
      // io.debug(candidates)
      let rules = parse_rules(rules)
      let candidates = parse_candidates(candidates)
      case rules {
        Ok(rules) -> Ok(Problem(rules, candidates))
        Error(e) -> Error(e)
      }
    }
    _ -> Error("Invalid input")
  }
}

// takes list:
// 1|2
// 2|3
// 2|4
// 3|4
// which means 1 must come before 2, 2 must come before 3, 2 must come before 4, 3 must come before 4
// and returns dict:
// { 2: [1], 3: [2], 4: [2, 3] }
// which means 2 must come after 1, 3 must come after 2, 4 must come after 2 and 3
fn parse_rules(input: String) -> Result(Rules, String) {
  // split input by new line
  let data: List(#(Int, Int)) =
    string.split(input, "\n")
    |> list.map(fn(line) {
      // io.debug(line)
      string.split_once(line, "|")
      |> result.unwrap(#("0", "0"))
      |> fn(tuple) {
        let #(key, value) = tuple
        // io.debug(key)
        // io.debug(value)
        let key = key |> string.trim |> int.parse |> result.unwrap(0)
        let value = value |> string.trim |> int.parse |> result.unwrap(0)

        #(value, key)
      }
    })

  // io.debug(data)

  let rules: Rules =
    list.fold(data, dict.new(), fn(dict, kvp) {
      let #(key, value) = kvp
      dict.upsert(dict, key, fn(existing_list) {
        case existing_list {
          option.Some(list) -> [value, ..list]
          option.None -> [value]
        }
      })
    })

  // io.debug(rules)

  Ok(rules)
}

fn parse_candidates(input: String) -> Candidates {
  string.split(input, "\n")
  |> list.map(fn(line) {
    string.split(line, ",")
    |> list.map(fn(num) { num |> int.parse |> result.unwrap(0) })
  })
}
