import gleam/result
import gleam/int
import gleam/io
import gleam/list

fn sample()
{
  2333133121414131402
}

pub fn main() {
  parse_disk(int.to_string(sample()))
}

pub type DiskBlock {
  File(id: Int, size: Int)
  Empty(size: Int)
}

pub type Disk = List(DiskBlock)

pub fn parse_disk(input: String) -> Disk {
  int.parse(input)
    |> result.unwrap(0)
    |> int.digits(10)
    |> result.unwrap([])
    |> list.index_map(fn(digit, i) {
      case i % 2 == 0 {
        True -> File(i/2, digit)
        False -> Empty(digit)
      }
    })
    |> io.debug

  // get forward facing and reverse facing disk
  // take all the empties, and map the reverse facing disk's files into the empties
  // discard leftovers
  // then replace the empties with our new file fragments
  // then do the checksum
  
}