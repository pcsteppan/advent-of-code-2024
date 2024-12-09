import gleam/result
import gleam/io
import day7
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

fn sample()
{
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"
}

pub fn part1_parse_sample_test() {
  io.debug("part1_parse_sample_test")
  let parse = day7.parse_problem(sample())
  let result = Ok([#(190, [10, 19]), #(3267, [81, 40, 27]), #(83, [17, 5]), #(156, [15, 6]), #(7290, [6, 8, 6, 15]), #(161011, [16, 10, 13]), #(192, [17, 8, 14]), #(21037, [9, 7, 18, 13]), #(292, [11, 6, 16, 20])])
  should.equal(parse, result)
}

pub fn part1_sample_test() {
  io.debug("part1_sample_test")
  let parse = day7.parse_problem(sample())
  case parse {
    Ok(problems) -> {
      let result = day7.solve_part1(problems)
      should.equal(result, 3749)
    }
    Error(_) -> {
      should.fail()
    }
  }
}

pub fn part1_test() {
  io.debug("part1_test")
  let input = day7.input()
    |> result.map_error(fn(e) { simplifile.describe_error(e) })
    |> result.try(fn(input) { day7.parse_problem(input) |> result.map_error(fn(_) { "Internal error" }) })

  result.try(input, fn(input) { Ok(day7.solve_part1(input)) })
    |> io.debug
    |> should.equal(Ok(6231007345478))
}

pub fn part2_sample_test() {
  io.debug("part1_sample_test")
  let parse = day7.parse_problem(sample())
  case parse {
    Ok(problems) -> {
      let result = day7.solve_part2(problems)
      should.equal(result, 11387)
    }
    Error(_) -> {
      should.fail()
    }
  }
}

// part2 handled in main since it takes awhile