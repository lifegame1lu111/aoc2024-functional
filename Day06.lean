import Mathlib.Data.Finset.Dedup

structure Course where
    tiles : Array (Array (Char × Bool))
    start : Nat × Nat
    dimensions : Nat × Nat
deriving Repr

inductive Direction | N | S | E | W deriving BEq

def labelTiles (dimensions : Nat × Nat) (tiles : List String) : Array (Array (Char × Bool)) :=
    let rec helper : List Char → Nat × Nat → List (Char × Bool)
    | [], _ => []
    | c :: cs, ⟨x, y⟩ => 
        let isBorder := 
            x == 0 
            || y == 0 
            || y == dimensions.snd - 1 
            || cs.isEmpty

        ⟨c, isBorder⟩ :: helper cs ⟨x + 1, y⟩

    let rec go : List String → Nat × Nat → List (Array (Char × Bool))
    | [], _ => []
    | line :: lines, pos@⟨x, y⟩ => (helper line.toList pos |>.toArray) :: go lines ⟨x, y + 1⟩

    go tiles ⟨0, 0⟩ |>.toArray

def Course.populate (content : String) : Course :=
    let lines := content.splitOn "\n"

    let dimensions := ⟨lines[0]!.length, lines.length - 1⟩
    let tiles := labelTiles dimensions lines

    let y := lines.findIdx (·.contains '^')
    let x? := tiles[y]!.findIdx? (·.fst == '^')
    
    match x? with
    | some x => Course.mk tiles ⟨x, y⟩ dimensions
    | none => Course.mk #[] ⟨0, 0⟩ ⟨0, 0⟩

partial def part1 (course : Course) : Nat :=
    let deltas : Array (Nat → Nat → Nat × Nat) := #[
        (⟨· + 0, · - 1⟩), 
        (⟨· + 0, · + 1⟩), 
        (⟨· + 1, · + 0⟩), 
        (⟨· - 1, · + 0⟩)
    ]

    let rec traverse (direction : Direction) (start : Nat × Nat) : List (Nat × Nat) :=
        let ⟨x, y⟩ := start
        let ⟨current, isBorder⟩ := course.tiles[y]![x]!

        if isBorder && current != '#' then [start]
        else
        if current == '#' 
        then
            match direction with
            | .N => traverse .E ⟨x + 1, y + 1⟩
            | .S => traverse .W ⟨x - 1, y - 1⟩
            | .E => traverse .S ⟨x - 1, y + 1⟩
            | .W => traverse .N ⟨x + 1, y - 1⟩
        else start :: traverse direction (deltas[direction.toCtorIdx]!.uncurry start)
    
    traverse .N course.start
    |>.dedup
    |>.length

partial def part2Aux (course : Course) : Nat :=
    let deltas : Array (Nat → Nat → Nat × Nat) := #[
        (⟨· + 0, · - 1⟩), 
        (⟨· + 0, · + 1⟩), 
        (⟨· + 1, · + 0⟩), 
        (⟨· - 1, · + 0⟩)
    ]

    let rec isLoopFrom (direction : Direction) (start : Nat × Nat) (prevSteps : List (Nat × Nat × Direction)) : Bool :=
        let ⟨x, y⟩ := start
        let ⟨current, isBorder⟩ := course.tiles[y]![x]!

        let currentPos := ⟨x, y, direction⟩
        
        if prevSteps.contains currentPos then true 
        else
        if isBorder && current != '#' then false
        else
        if current == '#'
        then
            match direction with
            | .N => isLoopFrom .E ⟨x + 1, y + 1⟩ (currentPos :: prevSteps)
            | .S => isLoopFrom .W ⟨x - 1, y - 1⟩ (currentPos :: prevSteps)
            | .E => isLoopFrom .S ⟨x - 1, y + 1⟩ (currentPos :: prevSteps)
            | .W => isLoopFrom .N ⟨x + 1, y - 1⟩ (currentPos :: prevSteps)
        else isLoopFrom direction (deltas[direction.toCtorIdx]!.uncurry start) (currentPos :: prevSteps)

    isLoopFrom .N course.start [] |>.toNat

partial def part2 (course : Course) : Nat :=
    let rec genCoursesAux (start : Nat × Nat) : List Course :=
        let ⟨x, y⟩ := start
        
        if x == course.dimensions.fst then []
        else
            let line := course.tiles[y]!
            let ⟨c, isBorder⟩ := line[x]!
        if c == '#' then genCoursesAux ⟨x + 1, y⟩
        else
            let line' := if c != '^' then line.set! x ⟨'#', isBorder⟩ else line
            let tiles' := course.tiles.set! y line'

            { course with tiles := tiles' } :: genCoursesAux ⟨x + 1, y⟩
            
    let rec genCourses (start : Nat × Nat) : List Course :=
        let ⟨x, y⟩ := start

        if y == course.dimensions.snd then []
        else genCoursesAux start ++ genCourses ⟨x, y + 1⟩
    
    let courses := genCourses ⟨0, 0⟩

    courses.map (part2Aux ·) |>.sum

def main : IO Unit := do
    let content ← IO.FS.readFile "input6.txt"
    let course := Course.populate content

    let resultPart1 := part1 course
    let resultPart2 := part2 course
    IO.println s!"Part 1: {resultPart1}"
    IO.println s!"Part 2: {resultPart2}"
