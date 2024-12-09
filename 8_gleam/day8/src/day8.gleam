import gleam/int
import simplifile
import gleam/result
import gleam/set
import gleam/dict
import gleam/list
import gleam/string
import gleam/io

pub type Vec2 = #(Int, Int)
pub type Antenna = #(String, Vec2)
pub type Antennas = List(Antenna)

fn split_into_lines(input: String) -> List(String) {
  // replace \r\n with \n
  input 
    |> string.replace("\r\n", "\n")
    |> string.split("\n")
}

pub fn parse_antennas(input: String) -> Antennas {
  input 
    |> split_into_lines
    |> list.index_map(fn(line, row) {
      line
        |> string.split("")
        |> list.index_map(fn(char, col) {
          #(char, #(row, col))
        })
        |> list.filter(fn(antenna) { antenna.0 != "." })
    })
    |> list.flatten
}

pub fn get_dimensions(input: String) -> Vec2 {
  let lines = split_into_lines(input)
  let height = lines |> list.length
  let width = lines |> list.first |> result.unwrap("") |> string.length
  #(height, width)
}

pub fn solve_helper(antennas: List(Antenna), dimensions: #(Int, Int)) {
  list.group(antennas, fn(antenna) {antenna.0})
    |> dict.map_values(fn(_, values) {
      find_all_antinodes(list.map(values, fn(antenna) {antenna.1}), int.max(dimensions.0, dimensions.1)) 
    })
    |> dict.values
    |> list.flatten
    |> list.filter(fn(antenna) {in_rect(antenna, dimensions)})
    |> set.from_list
}

pub fn main() {
   let input = simplifile.read("src/data.txt") |> result.unwrap("")

  let antennas = parse_antennas(input)
  let dimensions = get_dimensions(input)

  let result = solve_helper(antennas, dimensions)

  result |> set.size |> io.debug
}


fn in_rect(pos: Vec2, end: Vec2) -> Bool {
  let #(row, col) = pos
  let #(end_row, end_col) = end
  
  row >= 0 && row < end_row && col >= 0 && col < end_col
}

pub fn find_all_antinodes(antennas_of_same_type: List(Vec2), repeats: Int) -> List(Vec2) {
  // find all combinations of two
  list.combination_pairs(antennas_of_same_type)
    |> list.map(fn(positions){
      let #(pos1, pos2) = positions
      find_antinodes(pos1, pos2, repeats)
    })
    |> list.flatten
}

fn vec2_diff(a: Vec2, b: Vec2) -> Vec2 {
  #(a.0 - b.0, a.1 - b.1)
}

fn vec2_add(a: Vec2, b: Vec2) -> Vec2 {
  #(a.0 + b.0, a.1 + b.1)
}

fn vec2_negate(a: Vec2) -> Vec2 {
  #(-a.0, -a.1)
}

/// Finds all antinodes in a regularly repeating linear fashion
/// Includes the position of the antenna itself (since part 2 involves that modification)
/// 
pub fn find_antinodes(a: Vec2, b: Vec2, repeat: Int) -> List(Vec2) {
  let difference = vec2_diff(a, b)
  let negative_difference = vec2_negate(difference)

  let antinodes1 = generate_antinodes(a, difference, repeat)
  let antinodes2 = generate_antinodes(b, negative_difference, repeat)

  antinodes1 |> list.append(antinodes2)
}

fn generate_antinodes(start: Vec2, step: Vec2, repeat: Int) -> List(Vec2) {
  list.fold(list.range(0, repeat), [], fn(acc, _) {
    case acc {
      [prev] | [prev, .._] -> {
        let next = vec2_add(prev, step)
        [next, ..acc]
      }
      [] -> [start]
    }
  })
}
