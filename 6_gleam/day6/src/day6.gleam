import gleam/int
import gleeunit/should
import gleam/bool
import gleam/result
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import simplifile
import gleam/otp/task

pub type DateTime
// An external function that creates an instance of the type
@external(erlang, "os", "timestamp")
pub fn now() -> DateTime

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
  Scene(guy: Guy, obstacles: Set(Vec2), visited: Set(Guy), width: Int, height: Int)
}

pub type SceneChar {
  GuyChar(Vec2)
  ObstacleChar(Vec2)
}

pub type LineOutput = #(List(Vec2), Option(Guy))

fn clean_input(string) -> String {
  string.replace(string, "\r\n", "\n")
  |> string.trim
}

pub fn parse_scene(input: String) -> Result(Scene, String) {
  let cleaned_input = clean_input(input)

  let width = cleaned_input
    |> string.split("\n")
    |> list.first
    |> result.unwrap("")
    |> string.length

  let height = cleaned_input
    |> string.split("\n")
    |> list.length

  let #(obstacles, guy) =
    cleaned_input
    |> string.split("\n")
    |> list.index_map(get_positions_and_maybe_guy)
    |> list.fold(#([], None), fn(acc, curr: LineOutput) {
      let #(obstacles1, prev_guy) = acc
      let #(obstacles2, maybe_guy) = curr
      let merged_obstacles = list.append(obstacles1, obstacles2)

      case maybe_guy {
        Some(guy) -> #(merged_obstacles, Some(guy))
        None -> #(merged_obstacles, prev_guy)
      }
    })

  case guy, obstacles {
    Some(guy), obstacles -> Ok(Scene(guy, set.from_list(obstacles), set.new(), width, height))
    None, _ -> Error("No guy found in scene")
  }
}

pub fn solve_part1(scene: Scene) -> Result(Int, CycleError) {
  find_total_visited(scene)
}

pub fn solve_part2(scene: Scene) -> Int {
  // find all visited positions, create scene variants for each of the positions in the visited positions set
  // for each of those scene variants, add an obstacle for that position
  // tally how many of those scene variants result in a cycle error
  let original_guy = scene.guy
  let solution = find_completed_scene(scene)
  
  use <- bool.guard(result.is_error(solution), 0)
  
  let answer = result.try(solution, fn(scene) { 
    let Scene(guy, _, visited, width, height) = scene

    let visited_positions = visited
      |> set.to_list
      |> list.map(fn(guy) { guy.pos })
      |> list.filter(fn(pos) { pos != guy.pos })
      |> set.from_list
      |> set.to_list

    let cycle_count = visited_positions
      |> list.map(fn(pos) {
        let new_obstacles = set.insert(scene.obstacles, pos)
        Scene(original_guy, new_obstacles, set.new(), width, height)
      })
      |> list.index_map(fn(scene, _i) {
        // io.debug(i)
        // make async / parallel
        spawn_task_find_total_visited(scene)
        })
      |> list.map(task.await_forever)
      |> list.filter(fn(scene) { result.is_error(scene) })
      |> list.length

    Ok(cycle_count)
  })

  result.unwrap(answer, 0)  
}

// assumes no cycles in scene
pub type CycleError {
  CycleError
}

pub fn find_completed_scene(scene: Scene) -> Result(Scene, CycleError) {
  let Scene(guy, obstacles, visited_set, width, height) = scene

  // base case 1: guy has already visited
  let already_visited = set.contains(visited_set, guy)
  use <- bool.guard(already_visited, Error(CycleError))

  // base case 2: guy is off the grid
  let off_grid = guy.pos.0 < 0
    || guy.pos.0 >= height
    || guy.pos.1 < 0
    || guy.pos.1 >= width

  case off_grid {
    True -> Ok(scene)
    False -> {
      let new_guy = get_new_guy(guy, obstacles)

      use <- bool.guard(set.contains(obstacles, new_guy.pos), Error(CycleError))

      let new_visited = set.insert(visited_set, guy)
      
      find_completed_scene(Scene(new_guy, obstacles, new_visited, width, height))
    }
  }
}

// tries out the next position, and accounts for hitting an obstacle
// by recursively turning the heading and trying again
// in reality, we only need to check the heading/new position at most twice, for this problem
fn get_new_guy(guy: Guy, obstacles: Set(Vec2)) -> Guy {
  let tentative_new_pos = case guy.heading {
    North -> #(guy.pos.0 - 1, guy.pos.1)
    East -> #(guy.pos.0, guy.pos.1 + 1)
    South -> #(guy.pos.0 + 1, guy.pos.1)
    West -> #(guy.pos.0, guy.pos.1 - 1)
  }

  // is there an obstacle at the new position?
  let hit_obstacle = set.contains(obstacles, tentative_new_pos)

  // if there is an obstacle, try again with new heading
  case hit_obstacle {
    True -> {
      let new_heading = case guy.heading {
        North -> East
        East -> South
        South -> West
        West -> North
      }

      get_new_guy(Guy(guy.pos, new_heading), obstacles)
    }
    False -> Guy(tentative_new_pos, guy.heading)
  }
}

fn find_total_visited(scene: Scene) -> Result(Int, CycleError) {
  let completed_scene = find_completed_scene(scene)
  result.try(completed_scene, fn(scene) { 
      let unique_positions = set.to_list(scene.visited) 
        |> list.map(fn(guy) { guy.pos }) 
        |> set.from_list

      Ok(set.size(unique_positions))
    })
}

fn get_positions_and_maybe_guy(
  line: String,
  row: Int,
) -> LineOutput {
  string.split(line, "")
  |> list.index_map(fn(char, col) {
    let pos = #(row, col)
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

fn input() -> Result(String, simplifile.FileError) {
  simplifile.read("test/input.txt")
}

pub fn main() {
  io.debug("main: solve part2")
  let current_time = now()
  // 1889 is too low
  // 1951 expected
  let input = input()
    |> result.map_error(fn(e) { simplifile.describe_error(e) })

  io.debug("solution")
  result.try(input, run_part2) 
    |> io.debug
    |> should.equal(Ok(1951))

  let final_time = now()
  io.debug(current_time)
  io.debug(final_time)
}

fn spawn_task_find_total_visited(scene: Scene) -> task.Task(Result(Int, CycleError)) {
  task.async(fn() {
    find_total_visited(scene)
  })
}

// pub fn main() {
//   // Run loads of threads, no problem
//   let results = list.range(0, 200_000)
//   |> list.map(spawn_task)
//   |> list.map(task.await_forever)
  
// }

fn run_part2(input: String) -> Result(Int, String) {
  
  case parse_scene(input)
  {
    Ok(scene) -> Ok(solve_part2(scene))
    Error(e) -> Error(e)
  }
}