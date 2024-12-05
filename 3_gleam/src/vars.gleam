import gleam/io
import gleam/regexp
import gleam/list
import gleam/option
import gleam/int
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let filepath = "src/data.txt"
  let sample = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  let data = simplifile.read(filepath) 
    |> result.unwrap("") 
    |> string.trim
    |> string.replace("\r\n", "")

  let input = data // or sample

  // PART 1
  let part1 = parse_and_calculate(input)
  io.debug(part1)

  // PART 2
  let input_with_ends = "do()" <> input <> "don't()"

  let assert Ok(re_filter) = regexp.from_string("do\\(\\)(.*?)don't\\(\\)")
  let part2 = regexp.scan(re_filter, input_with_ends)
    |> list.map(fn(match) {
      list.map(match.submatches, fn(submatch) {
        option.unwrap(submatch, "")
      })
    })
    |> list.flatten
    |> string.concat
    |> parse_and_calculate

  io.debug(part2)
}

pub fn parse_and_calculate(input) {
  let assert Ok(re) = regexp.from_string("mul\\(([0-9]+),([0-9]+)\\)")

  let total = regexp.scan(re, input)
    |> list.map(fn(match) {
      list.map(match.submatches, fn(submatch) {
        option.unwrap(submatch, "")
          |> int.parse
          |> result.unwrap(0)
      })
      |> fn(l) {
        case l {
          [a, b] -> a * b
          _ -> 0
        }
      }
    })
    |> list.fold(0, int.add)

  total
}