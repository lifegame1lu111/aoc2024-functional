import Mathlib.Data.Nat.Log

abbrev Equation := Int × List Int

inductive Op | Sub | Div | Split | Nop

def parse (content : String) : List Equation :=
    let numbers := content.splitOn "\n"
        |>.dropLast
        |>.map (·.splitOn ":")

    let toEquation (equation : List String) : Equation :=
        let calibrationResult := equation[0]!.toInt!
        let parts := equation[1]!
            |>.splitOn " "
            |>.drop 1
            |>.map (·.toInt!)

        ⟨calibrationResult, parts⟩

    numbers.map toEquation

def part1 (equations : List Equation) : Int :=
    let rec helper : List Int → Int → Op → Bool
    | [], 1, .Div => True
    | [], 0, .Sub => True
    | [], _, _ => False
    | part :: parts, n, _ => 
         if n % part == 0 
         then helper parts (n / part) .Div
              || helper parts (n - part) .Sub
         else helper parts (n - part) .Sub

    equations
    |>.filter (fun ⟨res, parts⟩ => helper parts.reverse res .Nop)
    |>.foldl (fun acc ⟨res, _⟩ => acc + res) 0 

def part2 (equations : List Equation) : Int :=
    let rec helper (parts : List Int) (result : Int) (n₀ : Int) : Bool :=
    match parts with
    | [] => n₀ == result
    | part :: rest => 
         let multiplier := 10^(Nat.log 10 part.natAbs + 1)

         helper rest result (n₀ * part)
         || helper rest result (n₀ + part)
         || helper rest result (n₀ * multiplier + part)
    
    equations
    |>.filter (fun ⟨res, parts⟩ => helper (parts.drop 1) res parts[0]!)
    |>.foldl (fun acc ⟨res, _⟩ => acc + res) 0

def main : IO Unit := do
    let content ← IO.FS.readFile "input7.txt"
    let parsed := parse content

    let resultPart1 := part1 parsed
    let resultPart2 := part2 parsed

    IO.println s!"Part 1: {resultPart1}"
    IO.println s!"Part 1: {resultPart2}"
