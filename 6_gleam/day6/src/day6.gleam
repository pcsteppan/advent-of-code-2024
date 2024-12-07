import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string

// row, column
pub type Vec2 =
  #(Int, Int)

pub type Guy {
  Guy(pos: Vec2, heading: Direction)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Scene {
  Scene(guy: Guy, obstacles: List(Vec2), visited: Set(Vec2))
}

pub type SceneChar {
  GuyChar(Vec2)
  ObstacleChar(Vec2)
}

pub fn parse_scene(input: String) -> Scene {
  let lines =
    string.split(input, "\r\n")
    |> list.index_map(fn(i, line) { get_positions_and_maybe_guy(line, i) })
    |> list.fold(#([], None), fn(acc, curr) {
      let #(obstacles, _) = acc
      let #(positions, maybe_guy) = curr
      case maybe_guy {
        Some(guy) -> Scene(guy, obstacles, Set.from_list(positions))
        None -> Scene(None, obstacles, Set.from_list(positions))
      }
    })
}

fn get_positions_and_maybe_guy(
  line: String,
  row: Int,
) -> #(List(Vec2), Option(Guy)) {
  string.split(line, "")
  |> list.index_map(fn(char, index) {
    let pos = #(row, index)
    case char {
      "#" -> Some(ObstacleChar(pos))
      "^" -> Some(GuyChar(pos))
      "." -> None
      _ -> None
    }
  })
  |> list.fold(#([], None), fn(state, maybe_char) {
    let #(positions, maybe_guy) = state
    case maybe_char {
      Some(ObstacleChar(pos)) -> #([pos, ..positions], maybe_guy)
      Some(GuyChar(pos)) -> #(positions, Some(Guy(pos, North)))
      None -> state
    }
  })
}

pub fn main() {
  io.println("Hello from day6!")
}
