import gleam/result
import gleeunit/should
import gleam/io
import gleeunit
import day6.{Scene,Guy,North}
import gleam/set.{type Set}
import gleam/dict
import simplifile

// import qcheck_gleeunit_utils/test_spec

pub fn main() {
  gleeunit.main()
  // solve_part1_test()
}

fn input() -> Result(String, simplifile.FileError) {
  simplifile.read("test/input.txt")
}

fn official_sample() -> String {
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
}

fn sample() -> String {
"#.#.
...#
..^.
#..#"
}

// gleeunit test functions end in `_test`
pub fn parse_scene_test() {
  io.debug("parse_scene_test")
  let input = sample()
  let result = day6.parse_scene(input)

  let expected = Ok(
      Scene(
        Guy(#(2, 2), North), 
        set.from_list([#(0, 2), #(0, 0), #(1, 3), #(3, 3), #(3, 0)]), 
        set.new(),
        4,
        4
      )
    )

  should.equal(result, expected)
}

pub fn solve_part1_sample_test() {
  io.debug("solve_part1_sample_test")
  run_part1(sample())
}

pub fn solve_part1_official_sample_test() {
  io.debug("solve_part1_official_sample_test")
  run_part1(official_sample())
}

pub fn solve_part1_test() {
  io.debug("solve_part1_test")
  let input = input()
    |> result.map_error(fn(e) { simplifile.describe_error(e) })

  result.try(input, run_part1)
    |> should.equal(Ok(5101))
}

fn run_part1(input: String) -> Result(Int, String) {
  case day6.parse_scene(input)
  {
    Ok(scene) -> day6.solve_part1(scene) |> result.map_error(fn(_) {"CycleError"})
    Error(e) -> Error(e)
  }
}

pub fn cycle_test() {
  io.debug("cycle_test")
  let sample = "####
#..#
#^.#
####"

  let result = run_part1(sample)
  let expected = Error("CycleError")

  should.equal(result, expected)
}

pub fn solve_part2_mini_sample_test() {
  io.debug("solve_part2_mini_sample_test")
  let mini_sample = ".#..
...#
....
.^#."

  run_part2(mini_sample) |> io.debug
}

pub fn solve_part2_official_sample_test() {
  io.debug("solve_part2_sample_test")
  let sample = official_sample()
  run_part2(sample) |> io.debug
}

// pub fn solve_part2_test() {
//   io.debug("solve_part2_test")
//   let input = input()
//     |> result.map_error(fn(e) { simplifile.describe_error(e) })

//   result.try(input, run_part2)
//     |> should.equal(Ok(0))
// }

// pub fn hello__test_() {
//   // And use the `test_spec.make` function here.
//   use <- test_spec.make
  

// }

fn run_part2(input: String) -> Result(Int, String) {
  case day6.parse_scene(input)
  {
    Ok(scene) -> Ok(day6.solve_part2(scene))
    Error(e) -> Error(e)
  } 
}

pub fn two_obstacles_test() {
  let example = "#.
.#
^."

  // use part1 solver to get the completed scene
  let scene = day6.parse_scene(example)
    |> result.try(fn(scene) { day6.solve_part1(scene) |> result.map_error(fn(_) {"CycleError"}) })

  case scene {
    Ok(val) -> {
      should.equal(val, 2)
    }
    Error(e) -> {
      io.debug("Error")
      io.print_error(e)
    }
  }
}