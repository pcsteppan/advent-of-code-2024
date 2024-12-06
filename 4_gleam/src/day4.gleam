import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/float
import gleam/int
import gleam/dict
import gleam/regexp
import simplifile

pub fn main() {
  let filepath = "src/data.txt"
  let data = simplifile.read(filepath) 
    |> result.unwrap("") 
    |> string.trim
    |> string.replace("\r\n", "")

  // should have 4 'abc's
  let sample = "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX"

  let small_sample = "ABC
DEF
GHI"

  let input = data // or sample

  let linearized_input = linearize_grid_string(input)

  let len = list.length(linearized_input)
  let width = int.square_root(len)
    |> result.unwrap(0.0)
    |> float.truncate
  
  // get traversals for across rows, forward and reverse
  let row_forward = traversal(len, 1, width)

  // get traversals for along columns, forward and reverse
  let col_forward = traversal(len, width, 0)

  // get diagonal traversals
  let diag_1 = diagonal_traversal_1(width)
    |> list.map(fn (diag) {list_2d_to_1d(diag, width)})
    |> list.intersperse([-1])
    |> list.flatten
  
  let diag_2 = diagonal_traversal_2(width)
    |> list.map(fn (diag) {list_2d_to_1d(diag, width)})
    |> list.intersperse([-1])
    |> list.flatten

  // map traversals to strings
  let combined =
    [
      map_to_traversal(linearized_input, row_forward),
      map_to_traversal(linearized_input, col_forward),
      map_to_traversal(linearized_input, diag_1),
      map_to_traversal(linearized_input, diag_2)
    ]
    |> list.intersperse(["__"])
    |> list.concat


  let count = count_occurrences(combined, "XMAS")

  // 2573
  io.debug(count)
}

pub fn linearize_grid_string(grid_string) {
  let grid = grid_string
    |> string.split("\n")
    |> list.map(fn(row) {
      row
        |> string.trim
        |> string.split("")
    })
    |> list.flatten

  grid
}

pub fn traversal(len, stride, width) {
  // doesn't really need to be a range
  let range = list.range(0, len - 2)

  list.fold(range, [0], fn(acc, curr) {
    let prev = list.first(acc) |> result.unwrap(curr)
    
    let is_wrapping = {prev + stride} > {len - 1}
    let next = case is_wrapping {
      True -> {prev + stride} % {len - 1}
      False -> {prev + stride}
    }

    // insert a separator in the traveral if we're starting a new column or row
    let insert_separator = is_wrapping || {width > 0 && next % width == 0}
    case insert_separator {
      True -> [next, -1, ..acc]
      False -> [next, ..acc]
    }
  })
}

pub fn gen_diag_1(x_off, y_off, i) {
  let diag = y_off - int.absolute_value(i - x_off)
  
  list.map(
      list.range(0, diag - 1),
      fn (j) {
        let n = diag - 1
        let m = int.max(0, i - y_off + 1)
        #(n - j + m, j + m)
      }
    )
}

pub fn list_2d_to_1d(list_2d, width) {
  list.map(list_2d, fn (tuple: #(Int, Int)) {
    let row = tuple.0
    let col = tuple.1
    row * width + col
  })
}

pub fn diagonal_traversal_1(size) {
  let x_off = size - 1
  let y_off = size

  list.map(
    list.range(0, size * 2 - 2),
    fn (i) { gen_diag_1(x_off, y_off, i) }
  )
}

pub fn diagonal_traversal_2(size) {
  let normal_diag_traversal = diagonal_traversal_1(size)
  // flip column (y) values to get the other diagonal
  normal_diag_traversal
    |> list.map(fn (list) {
      list.map(list, fn (tuple: #(Int, Int)) {
        let row = tuple.0
        let col = tuple.1
        #(row, size - 1 - col)
      })
    })
}

pub fn map_to_traversal(input, traversal) {
  let input_map = list.index_map(input, fn(c, i) { #(i, c) })
    |> dict.from_list

  list.map(traversal, fn(i) { dict.get(input_map, i) |> result.unwrap("_") })
}

pub fn count_occurrences(input_list, substring) {
  let str = string.concat(input_list)

  let occurrences = get_match_count(substring, str)
  let occurrences_reverse = get_match_count(string.reverse(substring), str)

  occurrences + occurrences_reverse
}

pub fn get_match_count(re_str, str) {
  let assert Ok(re) = regexp.from_string(re_str)
  let occurrences = regexp.scan(re, str) |> list.length
  occurrences
}

