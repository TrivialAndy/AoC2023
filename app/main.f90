program main
  use aoc2023__day01
  use aoc2023__day02
  use aoc2023__day03
  use aoc2023__day04
  use aoc2023__day05
  use aoc2023__day06
  use aoc2023__day07
  use aoc2023__day08
  use aoc2023__day09
  use aoc2023__day10
  use aoc2023__day11
  use aoc2023__day12
  use aoc2023__day13
  use aoc2023__day14
  use aoc2023__day15
  use aoc2023__day16
  use aoc2023__day17
  use aoc2023__day18
  use aoc2023__day19
  use aoc2023__day20
  use aoc2023__day21
  use aoc2023__day22
  use aoc2023__day23
  use aoc2023__day24
  use aoc2023__day25
  implicit none

  ! Get input (day, part, dataset)

  select case(day)
      case "01"
          call day01(part, dataset)
      case "02"
          call day02(part, dataset)
      case "03"
          call day03(part, dataset)
      case "04"
          call day04(part, dataset)
      case "05"
          call day05(part, dataset)
      case "06"
          call day06(part, dataset)
      case "07"
          call day07(part, dataset)
      case "08"
          call day08(part, dataset)
      case "09"
          call day09(part, dataset)
      case "10"
          call day10(part, dataset)
      case "11"
          call day11(part, dataset)
      case "12"
          call day12(part, dataset)
      case "13"
          call day13(part, dataset)
      case "14"
          call day14(part, dataset)
      case "15"
          call day15(part, dataset)
      case "16"
          call day16(part, dataset)
      case "17"
          call day17(part, dataset)
      case "18"
          call day18(part, dataset)
      case "19"
          call day19(part, dataset)
      case "20"
          call day20(part, dataset)
      case "21"
          call day21(part, dataset)
      case "22"
          call day22(part, dataset)
      case "23"
          call day23(part, dataset)
      case "24"
          call day24(part, dataset)
      case "25"
          call day25(part, dataset)
      case default
          print *, "Day not in advent! Are you solving problems from another date?!"
  end select

end program main
