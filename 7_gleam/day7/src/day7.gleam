import simplifile
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleam/io
import gleam/result

pub type Solution = Int
pub type Constants = List(Int)
pub type Problem = #(Solution, Constants)

pub type ParseError {
  ParseError
}

fn split_into_lines(input: String) -> List(String) {
  // replace \r\n with \n
  input 
    |> string.replace("\r\n", "\n")
    |> string.split("\n")
}

pub fn parse_problem(input: String) -> Result(List(Problem), ParseError) {
  let lines = split_into_lines(input)
    |> list.map(parse_line)

  result.all(lines)
}

fn parse_line(line: String) -> Result(Problem, ParseError) {
  let parts = line |> string.split_once(": ")

  case parts {
    Error(_) -> Error(ParseError)
    Ok(problem) -> {
      let solution = problem.0 |> int.parse
      let constants = problem.1 |> parse_constants

      case solution, constants {
        Error(_), _ | _, Error(_) -> Error(ParseError)
        Ok(solution), Ok(constants) -> Ok(#(solution, constants))
      }
    }
  }
}

fn parse_constants(constants: String) -> Result(Constants, ParseError) {
  let parse: List(Result(Int, _)) = constants
    |> string.split(" ")
    |> list.map(int.parse)
    |> list.map(result.map_error(_, fn(_) { ParseError }))
  
  result.all(parse)
}

pub fn solve_part1(problems: List(Problem)) -> Int {
  problems
    |> list.map(fn(problem) { #(problem.0, get_all_solutions_part1(problem.1, 0)) })
    |> list.filter(fn(solution) { list.any(solution.1, fn(x) { x == solution.0 }) })
    |> list.map(fn(solution) { solution.0 })
    |> list.fold(0, int.add)
}

pub fn solve_part2(problems: List(Problem)) -> Int {
  problems
    |> list.map(fn(problem) { #(problem.0, get_all_solutions_part2(problem.1, 0)) })
    |> list.filter(fn(solution) { list.any(solution.1, fn(x) { x == solution.0 }) })
    |> list.map(fn(solution) { solution.0 })
    |> list.fold(0, int.add)
}

/// Recursively applies either addition or multiplication between the running total and the next constant
/// until all constants have been applied
fn get_all_solutions_part1(constants: Constants, total: Int) -> List(Int) {
  case constants {
    [] -> [total]
    [constant, ..rest] -> {
      let new_sum = total + constant
      let new_futures = get_all_solutions_part1(rest, new_sum)

      let new_product = total * constant
      let new_futures2 = get_all_solutions_part1(rest, new_product)

      list.append(new_futures, new_futures2)
    }
  }
}

fn get_all_solutions_part2(constants: Constants, total: Int) -> List(Int) {
  case constants {
    [] -> [total]
    [constant, ..rest] -> {
      let new_sum = total + constant
      let new_futures = get_all_solutions_part2(rest, new_sum)

      let new_product = total * constant
      let new_futures2 = get_all_solutions_part2(rest, new_product)

      let length_of_constant = int.digits(constant, 10) |> result.unwrap([]) |> list.length
      let new_concat = total * {int.power(10, int.to_float(length_of_constant)) |> result.unwrap(1.) |> float.truncate} + constant
      let new_futures3 = get_all_solutions_part2(rest, new_concat)

      new_futures |> list.append(new_futures2) |> list.append(new_futures3)
    }
  }
}

pub fn input() -> Result(String, simplifile.FileError) {
  simplifile.read("test/data.txt")
}

pub fn main() {
  io.debug("part2_test")
  let input = input()
    |> result.map_error(fn(e) { simplifile.describe_error(e) })
    |> result.try(fn(input) { parse_problem(input) |> result.map_error(fn(_) { "Internal error" }) })

  result.try(input, fn(input) { Ok(solve_part2(input)) })
    |> io.debug
}