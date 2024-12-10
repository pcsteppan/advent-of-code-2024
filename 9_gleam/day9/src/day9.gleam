import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set

fn real() -> String {
  ""
}

pub type DiskData {
  EmptyBlock(Int)
  FileBlock(size: Int, id: Int)
}

pub fn main() {
  part2()
}

// PART 1

pub fn part1() {
  io.debug("part 1")
  let disk = parse_disk(real())
  let defragmented_disk = defragment_disk(disk)
  let checksum = calc_checksum(defragmented_disk)
  io.debug("part 1 checksum:")
  io.debug(int.to_string(checksum))
}

pub fn defragment_disk(disk: List(Int)) -> List(Int) {
  let reverse =
    list.reverse(disk)
    |> list.filter(fn(digit) { digit != -1 })

  let reverse_length = list.length(reverse)

  list.fold(disk, #(reverse, [], 0, reverse_length), fn(acc, curr) {
    let #(reverse, defrag, unchanged_count, remaining_reverse) = acc

    let complete = unchanged_count >= remaining_reverse

    case complete, curr, reverse {
      True, _, _ -> #([], [-1, ..defrag], unchanged_count, remaining_reverse)
      _, -1, [reverse_head, ..reverse_tail] -> {
        let defrag = [reverse_head, ..defrag]
        #(reverse_tail, defrag, unchanged_count, remaining_reverse - 1)
      }
      _, _, _ -> {
        let defrag = [curr, ..defrag]
        #(reverse, defrag, unchanged_count + 1, remaining_reverse)
      }
    }
  }).1
  |> list.reverse
}

pub fn calc_checksum(disk: List(Int)) -> Int {
  list.index_fold(disk, 0, fn(acc, curr, i) {
    case curr >= 0 {
      True -> i * curr + acc
      False -> acc
    }
  })
}

pub fn parse_disk(input: String) -> List(Int) {
  int.parse(input)
  |> result.unwrap(0)
  |> int.digits(10)
  |> result.unwrap([])
  |> list.index_map(fn(digit, i) {
    case i % 2 == 0 {
      True -> list.repeat(i / 2, digit)
      False -> list.repeat(-1, digit)
    }
  })
  |> list.flatten
}

// PART 2

pub fn part2() {
  io.debug("part 2")
  let disk = parse_disk_part2(real())
  let defragmented_disk = defragment_disk_part2(disk)
  let checksum = calc_checksum(defragmented_disk)
  io.debug("part 2 checksum:")
  io.debug(int.to_string(checksum))
}

pub fn defragment_disk_part2(disk: List(DiskData)) -> List(Int) {
  let file_blocks_in_reverse =
    list.filter(disk, fn(digit) {
      case digit {
        FileBlock(_, _) -> True
        _ -> False
      }
    })
    |> list.reverse

  let disk_with_copies =
    list.fold(file_blocks_in_reverse, disk, fn(acc, file_block) {
      copy_into_left_most_empty(acc, file_block)
    })

  let deduped_disk = remove_duplicates(disk_with_copies)

  let linearized_copy = linearize_disk(deduped_disk)

  linearized_copy
}

pub fn copy_into_left_most_empty(
  disk: List(DiskData),
  file: DiskData,
) -> List(DiskData) {
  list.fold(disk, #([], False), fn(acc, curr) {
    let #(new_disk, inserted_or_passed) = acc
    case inserted_or_passed, curr, file {
      True, _, _ -> #([curr, ..new_disk], True)
      False, EmptyBlock(size), FileBlock(file_size, file_id) -> {
        // the case where the file block is smaller than or equal the empty block
        case size >= file_size {
          True -> {
            let new_disk = [FileBlock(file_size, file_id), ..new_disk]
            let new_disk = case size > file_size {
              True -> [EmptyBlock(size - file_size), ..new_disk]
              False -> new_disk
            }
            #(new_disk, True)
          }
          False -> #([EmptyBlock(size), ..new_disk], False)
        }
      }
      False, FileBlock(_, id), FileBlock(_, file_id) -> {
        let same_file = id == file_id
        #([curr, ..new_disk], same_file)
      }
      _, _, _ -> #([curr, ..new_disk], False)
    }
  }).0
  |> list.reverse
}

pub fn remove_duplicates(disk: List(DiskData)) -> List(DiskData) {
  list.fold(disk, #([], set.new()), fn(acc, curr) {
    let #(new_disk, seen) = acc
    let seen_before = set.contains(seen, curr)
    case curr, seen_before {
      FileBlock(size, _), True -> {
        let new_empty = EmptyBlock(size)
        #([new_empty, ..new_disk], seen)
      }
      FileBlock(_, _), False -> {
        let new_seen = set.insert(seen, curr)
        #([curr, ..new_disk], new_seen)
      }
      _, _ -> #([curr, ..new_disk], seen)
    }
  }).0
  |> list.reverse
}

pub fn linearize_disk(disk: List(DiskData)) -> List(Int) {
  list.map(disk, fn(disk_data) {
    case disk_data {
      EmptyBlock(size) -> list.repeat(-1, size)
      FileBlock(size, id) -> list.repeat(id, size)
    }
  })
  |> list.flatten
}

pub fn parse_disk_part2(input: String) -> List(DiskData) {
  int.parse(input)
  |> result.unwrap(0)
  |> int.digits(10)
  |> result.unwrap([])
  |> list.index_fold([], fn(acc: List(DiskData), digit, idx) {
    case idx % 2 == 0 {
      True -> [FileBlock(digit, idx / 2), ..acc]
      False -> [EmptyBlock(digit), ..acc]
    }
  })
  |> list.reverse
}
