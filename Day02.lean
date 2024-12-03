abbrev Report := List Int

inductive Safety
    | Safe
    | Unsafe 
deriving BEq

def parse (content : String) : List Report := 
    let helper : String → Report
        | "" => [0]
        | list => list.splitOn |>.map (·.toInt!)
    
    content
    |>.splitOn "\n"
    |>.map helper

def checkSafety : Safety → List Int → Nat
    | Safety.Safe, _ :: [] => 1
    | _, d1 :: d2 :: ds => 
         if d1.sign != d2.sign 
         || d1.natAbs ∉ [1, 2, 3]
         || d2.natAbs ∉ [1, 2, 3] then 0
         else checkSafety Safety.Safe (d2 :: ds)
    | _, _ => 0

def part1Aux (report : Report) : Nat := report
    |>.zipWith (· - ·) report.tail
    |> checkSafety Safety.Unsafe

def part1 (reports : List Report) : Nat := reports
    |>.map part1Aux
    |>.sum

def part2Aux (report : Report) : Nat :=
    let isSafe (r : Report) : Bool := r
        |>.zipWith (· - ·) r.tail
        |> checkSafety Safety.Unsafe
        |> (· == 1)
    
    let rec helper : List Int → List (List Int)
        | [] => [[]]
        | d :: ds => ds :: (helper ds |>.map (d :: ·))

    helper report
    |>.any isSafe
    |>.toNat

def part2 (reports : List Report) : Nat := reports
    |>.map part2Aux
    |>.sum
       
def main : IO Unit := do
    let content ← IO.FS.readFile "input2.txt" 
    let parsed := parse content
    
    let resultPart1 := part1 parsed
    let resultPart2 := part2 parsed

    IO.println s!"Part 1: {resultPart1}"
    IO.println s!"Part 2: {resultPart2}"
