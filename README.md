# type_inference
This repositry is an implementation of [Hindley Milner Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) in Rust.
This inference algorithm is used in a variety of languages, including Haskell, Ocaml, and Rust, although implementation methods and details differ.

An Ocaml implementation already exists([type-inference](https://github.com/prakhar1989/type-inference)), but there was no Rust implementation (although there may be one), so I reimplemented it.
This repository is also an experimental project to create a type system for my language "elana".
To facilitate understanding and research of the algorithm, I modified the algorithm so that it can display a log of the inference computation process.
When you enter an expression like in ML languages, 
the system will infer the type of the expression and display the calculation process and the type assigned to the expression.

# Examples
```Shell
> cargo run "1 + 1"
input:
    1 + 1
annotated:
    (1:Num + 1:Num):a0
constraints:
    [a0 = Num]
process:
    Unify [a0 = Num] {
        Unify [] => []
        Apply a0 -> [] => a0
        Apply Num -> [] => Num
        UnifyOne a0 <-> Num => [(a0, Num)]
    } => [(a0, Num)]
output:
    (1:Num + 1:Num):Num
```

```Shell
> cargo run "(fun x -> x + 1)" 
input:
    fun x -> x + 1
annotated:
    (fun x -> (x:a0 + 1:Num):a1):(a0 -> a2)
constraints:
    [a0 = Num, a1 = Num, a1 = a2]
process:
    Unify [a0 = Num, a1 = Num, a1 = a2] {
        Unify [a0 = Num, a1 = Num] {
            Unify [a0 = Num] {
                Unify [] => []
                Apply a0 -> [] => a0
                Apply Num -> [] => Num
                UnifyOne a0 <-> Num => [(a0, Num)]
            } => [(a0, Num)]
            Apply a1 -> [(a0, Num)] {
                Substitude (Num, a0, a1) => a1
            } => a1
            Apply Num -> [(a0, Num)] {
                Substitude (Num, a0, Num) => Num
            } => Num
            UnifyOne a1 <-> Num => [(a1, Num)]
        } => [(a0, Num), (a1, Num)]
        Apply a1 -> [(a0, Num), (a1, Num)] {
            Substitude (Num, a0, a1) => a1
            Substitude (Num, a1, a1) => Num
        } => Num
        Apply a2 -> [(a0, Num), (a1, Num)] {
            Substitude (Num, a0, a2) => a2
            Substitude (Num, a1, a2) => a2
        } => a2
        UnifyOne Num <-> a2 => [(a2, Num)]
    } => [(a0, Num), (a1, Num), (a2, Num)]
output:
    (fun x -> (x:Num + 1:Num):Num):(Num -> Num)
```

```Shell
> cargo run "(fun x -> x + 1) 2"
input:
    (fun x -> x + 1) 2
annotated:
    (((fun x -> (x:a0 + 1:Num):a1):(a0 -> a2)) 2:Num):a3
constraints:
    [a0 = Num, a1 = Num, a1 = a2, a3 = a2, a0 = Num]
process:
    Unify [a0 = Num, a1 = Num, a1 = a2, a3 = a2, a0 = Num] {
        Unify [a0 = Num, a1 = Num, a1 = a2, a3 = a2] {
            Unify [a0 = Num, a1 = Num, a1 = a2] {
                Unify [a0 = Num, a1 = Num] {
                    Unify [a0 = Num] {
                        Unify [] => []
                        Apply a0 -> [] => a0
                        Apply Num -> [] => Num
                        UnifyOne a0 <-> Num => [(a0, Num)]
                    } => [(a0, Num)]
                    Apply a1 -> [(a0, Num)] {
                        Substitude (Num, a0, a1) => a1
                    } => a1
                    Apply Num -> [(a0, Num)] {
                        Substitude (Num, a0, Num) => Num
                    } => Num
                    UnifyOne a1 <-> Num => [(a1, Num)]
                } => [(a0, Num), (a1, Num)]
                Apply a1 -> [(a0, Num), (a1, Num)] {
                    Substitude (Num, a0, a1) => a1
                    Substitude (Num, a1, a1) => Num
                } => Num
                Apply a2 -> [(a0, Num), (a1, Num)] {
                    Substitude (Num, a0, a2) => a2
                    Substitude (Num, a1, a2) => a2
                } => a2
                UnifyOne Num <-> a2 => [(a2, Num)]
            } => [(a0, Num), (a1, Num), (a2, Num)]
            Apply a3 -> [(a0, Num), (a1, Num), (a2, Num)] {
                Substitude (Num, a0, a3) => a3
                Substitude (Num, a1, a3) => a3
                Substitude (Num, a2, a3) => a3
            } => a3
            Apply a2 -> [(a0, Num), (a1, Num), (a2, Num)] {
                Substitude (Num, a0, a2) => a2
                Substitude (Num, a1, a2) => a2
                Substitude (Num, a2, a2) => Num
            } => Num
            UnifyOne a3 <-> Num => [(a3, Num)]
        } => [(a0, Num), (a1, Num), (a2, Num), (a3, Num)]
        Apply a0 -> [(a0, Num), (a1, Num), (a2, Num), (a3, Num)] {
            Substitude (Num, a0, a0) => Num
            Substitude (Num, a1, Num) => Num
            Substitude (Num, a2, Num) => Num
            Substitude (Num, a3, Num) => Num
        } => Num
        Apply Num -> [(a0, Num), (a1, Num), (a2, Num), (a3, Num)] {
            Substitude (Num, a0, Num) => Num
            Substitude (Num, a1, Num) => Num
            Substitude (Num, a2, Num) => Num
            Substitude (Num, a3, Num) => Num
        } => Num
        UnifyOne Num <-> Num => []
    } => [(a0, Num), (a1, Num), (a2, Num), (a3, Num)]
output:
    (((fun x -> (x:Num + 1:Num):Num):(Num -> Num)) 2:Num):Num
```
