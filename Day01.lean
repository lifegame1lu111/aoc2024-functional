import Batteries.Data.String
import Mathlib.Data.List.Sort

def parse (content : String) : List Int × List Int :=
    let numbers := content
        |>.replace "   " " "
        |>.split (fun c => c == ' ' || c == '\n')

    let rec helper : List Int × List Int → List String → List Int × List Int
        | res, x1 :: x2 :: xs => helper ⟨x1.toInt! :: res.fst, x2.toInt! :: res.snd⟩ xs
        | res, _ => ⟨res.fst.mergeSort, res.snd.mergeSort⟩

    helper ⟨[], []⟩ numbers

def part1 (ns : List Int) (ms : List Int) : Nat := ns
    |>.zipWith (· - ·) ms
    |>.foldl (· + ·.natAbs) (0 : Nat)

def part2 (ns : List Int) (ms : List Int) : Nat := ns
    |>.foldl (fun acc n => acc + (n.toNat * (ms |>.filter (· == n) |>.length))) (0 : Nat)

def main : IO Unit := do
    let content ← IO.FS.readFile "input1.txt"
    let parsed := parse content

    let resultPart1 := part1.uncurry parsed
    let resultPart2 := part2.uncurry parsed

    IO.println s!"Part 1: {resultPart1}"
    IO.println s!"Part 2: {resultPart2}"
