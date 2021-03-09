fun rangePrime 0 = [ 0 ]
  | rangePrime x = x :: rangePrime (x - 1)

fun reverse nil = nil
  | reverse (h::t) = reverse t @ [ h ]

val range =
    (reverse o rangePrime)

fun head xs =
    case xs of
        head::tail => head

fun rest xs =
    case xs of
        _::tail => tail

fun modC a =
    fn b => a mod b

fun zero n =
    case n of
        0 => true
      | _ => false

fun isDivisible xs =
    fn n =>
       let val result = foldl (fn (a,b) => a orelse b) false (map zero (map (modC n) xs))
       in (result, n) end

fun sum (factors: int list, limit: int) =
    (foldl (op+) 0 o
     map (fn (_, v) => v) o
     List.filter (fn (b, _) => b) o
     map (isDivisible factors) o
     rest)
        (range (limit - 1))
