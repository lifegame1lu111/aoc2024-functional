import Batteries.Data.String
import Mathlib.Data.List.Sort
import Init.Data.String

def parse (content : String) : List ℤ × List ℤ :=
    let numbers := content
        |>.replace "   " " "
        |>.split (fun c => c == ' ' || c == '\n')

    let rec helper : List ℤ × List ℤ → List String → List ℤ × List ℤ
        | res, x1 :: x2 :: xs => helper ⟨x1.toInt! :: res.fst, x2.toInt! :: res.snd⟩ xs
        | res, _ => ⟨res.fst.mergeSort, res.snd.mergeSort⟩

    helper ⟨[], []⟩ numbers

def part1 (ns : List ℤ) (ms : List ℤ) : ℕ := ns
    |>.zipWith (· - ·) ms
    |>.foldl (fun acc n => acc + n.natAbs) (0 : ℕ)

def part2 (ns : List ℤ) (ms : List ℤ) : ℕ := ns
    |>.foldl (fun acc n => acc + (n.natAbs * (ms |>.filter (· == n) |>.length))) (0 : ℕ)

partial def main : IO Unit := do
    let content ← IO.FS.readFile "input1.txt"
    let parsed := parse content

    let resultPart1 := part1.uncurry parsed
    let resultPart2 := part2.uncurry parsed

    IO.println s!"Part 1: {resultPart1}"
    IO.println s!"Part 2: {resultPart2}"
