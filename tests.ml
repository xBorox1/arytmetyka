#load "arytmetyka.cmo";;
open Arytmetyka;;

let jeden = wartosc_dokladna 1.;;
let zero = wartosc_dokladna 0.;;
let is_nan x = classify_float x = FP_nan

let x = wartosc_dokladnosc 2. 50.;;

assert( (min_wartosc x, max_wartosc x) = (1., 3.) )

let x = wartosc_dokladnosc (-2.) 50.;;

assert( (min_wartosc x, max_wartosc x) = ((-3.), (-1.)) )

let x = wartosc_od_do 1. 3.;;
let y = wartosc_od_do 4. 5.;;
let z = plus x y;;

assert( (min_wartosc z, max_wartosc z) = (5., 8.));;
assert( (in_wartosc z 6.) = true);;

let x = wartosc_od_do (-1.) 1.
let y = podzielic jeden x;;

assert( (min_wartosc y, max_wartosc y) = (neg_infinity, infinity) );;
assert( in_wartosc y 0. = false);;
assert( in_wartosc y 10. = true);;
assert( is_nan (sr_wartosc y) = true );;

let y = podzielic jeden zero;;

assert( (is_nan (min_wartosc y), is_nan (max_wartosc y), is_nan (sr_wartosc y)) = (true, true, true) );; 

let y = razy y zero;;

assert( (is_nan (min_wartosc y), is_nan (max_wartosc y), is_nan (sr_wartosc y)) = (true, true, true) );; 

let x = wartosc_od_do 0. 1.;;
let x = podzielic jeden x;; 
let zerinf = minus x jeden;;
let minjeden = minus zero jeden;;
let infzer = razy zerinf minjeden;;
let inf = plus zerinf infzer;;

assert( (min_wartosc inf, max_wartosc inf) = (neg_infinity, infinity));;

let x = razy infzer infzer;;

assert( (min_wartosc x, max_wartosc x) = (0., infinity));;

let x = razy infzer zerinf;;

assert( (min_wartosc x, max_wartosc x) = (neg_infinity, 0.));;

let x = podzielic inf inf;;

assert( (min_wartosc x, max_wartosc x) = (neg_infinity, infinity));;

let x = wartosc_od_do (-1.) 1.;;
let x = podzielic jeden x;;
let x = razy x x;;

assert( (min_wartosc x, max_wartosc x) = (neg_infinity, infinity));;
assert( in_wartosc x 0. = false)

let x = podzielic x x;;

assert( (min_wartosc x, max_wartosc x) = (neg_infinity, infinity));;
assert( in_wartosc x 0. = true)

let x = wartosc_od_do (-1.) 1.;;
let x = podzielic jeden x;;
let x = plus jeden x;;
let x = plus jeden x;;
let x = podzielic jeden x;;

assert( (min_wartosc x, max_wartosc x) = (neg_infinity, infinity));;
assert( in_wartosc x 1. = true);;
assert( in_wartosc x 0. = true);;
assert( in_wartosc x 0.5 = false);;
