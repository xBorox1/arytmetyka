(* Autor : Michał Borowski *)
(* Reviewer : Wojciech Kłopotek *)

(**********************)
(* SPECYFIKACJA TYPÓW *)
(**********************)

(* Typ wartosc jest określony przez zamknięty przedział wartości *)
(* które może przyjmować wyrażenie*)
(* LUB *)
(* przez otwarty przedział wartości, których nie może przyjmować *)
(* wyrażenie, pod warunkiem, że jest niepusty i ma skończoną *)
(* długość - wtedy lewy > prawy *)
(* Przedział pusty jest określony przez (nan, nan). *)
type wartosc = {lewy : float ; prawy : float}

(**********************)
(*       STAŁE        *)
(**********************)

let nieskonczony_przedzial =
        {lewy = neg_infinity ; prawy = infinity}

let zerowy_przedzial =
        {lewy = 0. ; prawy = 0.}

let pusty_przedzial = 
        {lewy = nan ; prawy = nan}

(**********************)
(*    KONSTRUKTORY    *)
(**********************)

let wartosc_dokladnosc x p =
        let p = p *. 0.01
        in if x > 0. then
                {lewy = x -. x *. p ;
                 prawy = x +. x *. p}
        else
                {lewy = x +. x *. p ;
                 prawy = x -. x *. p}

let wartosc_od_do x y = {lewy = x ; prawy = y}

let wartosc_dokladna x = wartosc_od_do x x

(**********************)
(*     SELEKTORY      *)
(**********************)

(* Funkcja sprawdzająca, czy wartosc to przedział wartośći *)
(* liczb których brak w zbiorze możliwych wartości. *)
let brak war =
        if war.lewy > war.prawy then true
        else false

let in_wartosc war x =
        if brak war then 
                (war.lewy <= x) ||
                (war.prawy >= x)
        else
                (war.lewy <= x) &&
                (war.prawy >= x)

let min_wartosc war =
        if brak war then neg_infinity
        else war.lewy

let max_wartosc war =
        if brak war then infinity
        else war.prawy

let sr_wartosc war = 
        (min_wartosc war +. max_wartosc war) /. 2.

(************************)
(*  FUNKCJE POMOCNICZE  *)
(************************)

let is_nan x =
        classify_float x = FP_nan

let czy_pusty war =
        is_nan war.lewy

open List

(* f]Funkcja zwracająca minimalną wartość z listy floatów, *)
(* przy czym pomija nany. *)
let minimum lst = 
        fold_left (
                fun acc element ->
                    if is_nan element then acc
                    else if is_nan acc then element
                    else min acc element)
                nan lst (* nan - akumulator na początku *)

(* Funkcja analogiczna do minimum. *)
let maximum lst = 
        fold_left (
                fun acc element ->
                    if is_nan element then acc
                    else if is_nan acc then element
                    else max acc element)
                nan lst

(* Funkcja konwertująca -0 do 0, *)
(* dla innej liczby zwraca ją samą. *)
let norm_zero x = 
        if x = (-0.) then 0.
        else x

(* Funkcja dostaje przedział brakujących wartości i *)
(* sprawdza czy jakichkolwiek wartości brakuje. *)
let sprawdz_brak war =
        if war.lewy <= war.prawy 
        then nieskonczony_przedzial
        else war

(* Dostaje przedział brakujących wartości i zwraca dwa *)
(* przedziały wartości możliwych. *)
let rozbij_brak war =
        let war1 = wartosc_od_do neg_infinity war.prawy
        in let war2 = wartosc_od_do war.lewy infinity
        in (war1, war2)

(* Dostaje dwa nieskończone przedziały - jeden zaczynający się *)
(* od neg_infinity, a drugi kończący się na infinity, zwraca wartosc ich sumy. *)
let rec suma_wartosci_nieskonczonych war1 war2 =
        if war1.prawy > war2.prawy then 
                suma_wartosci_nieskonczonych war2 war1
        else sprawdz_brak {lewy = war2.lewy ; prawy = war1.prawy}
        

(************************)
(* FUNKCJE ARYTMETYCZNE *)
(************************)

let przeciwnosc war =
        wartosc_od_do ((-1.) *. war.prawy) ((-1.) *. war.lewy)

(* Zwraca odwrtoność floata, ale dla liczby 0 zwraca 0. *)
let odw x = 
        if x = 0. then 0.
        else (1. /. x)

let odwrotnosc war = 
        let br = brak war
        in if not br then
                if war = nieskonczony_przedzial then nieskonczony_przedzial
                else if not (in_wartosc war 0.) then
                        let odwlew = 1. /. war.lewy
                        in let odwpraw = 1. /. war.prawy
                        in let mini = min odwlew odwpraw
                        in let maxi = max odwlew odwpraw
                        in wartosc_od_do mini maxi
                else
                        if war.lewy = 0. && war.prawy = 0. then pusty_przedzial
                        else if war.lewy = 0. then wartosc_od_do (1. /. war.prawy) infinity
                        else if war.prawy = 0. then wartosc_od_do neg_infinity (1. /. war.lewy)
                        else wartosc_od_do (1. /. war.prawy) (1. /. war.lewy)
        else
                if not (in_wartosc war 0.) then
                        wartosc_od_do (1. /. war.prawy) (1. /. war.lewy)
                else
                        let odwlew = odw war.lewy
                        in let odwpraw = odw war.prawy
                        in let mini = min odwlew odwpraw
                        in let maxi = max odwlew odwpraw
                        in wartosc_od_do maxi mini

let plus war1 war2 = 
        let lew = war1.lewy +. war2.lewy
        in let praw = war1.prawy +. war2.prawy
        in let br1 = brak war1
        in let br2 = brak war2
        in let wynik = wartosc_od_do lew praw
        in
                if not br1 && not br2 then wynik
                else if br1 && br2 then nieskonczony_przedzial 
                else sprawdz_brak wynik

let minus war1 war2 = plus war1 (przeciwnosc war2)

(* Mnoży wartości określone przez przedziały możliwych wartości. *)
let razy_niebrak war1 war2 =
        let il1 = war1.lewy *. war2.prawy
        in let il2 = war1.prawy *. war2.lewy
        in let il3 = war1.lewy *. war2.lewy
        in let il4 = war1.prawy *. war2.prawy
        in let lst = [il1 ; il2 ; il3 ; il4]
        in let minimum = minimum lst
        in let maximum = maximum lst
        in {lewy = norm_zero minimum ; prawy = norm_zero maximum}

(* Mnoży wartości określone przez przedziały brakujących wartości. *)
let razy_brak war1 war2 =
        let il1 = war1.lewy *. war2.lewy
        in let il2 = war1.lewy *. war2.prawy
        in let il3 = war1.prawy *. war2.lewy
        in let il4 = war1.prawy *. war2.prawy
        in let maxi = max il3 il2
        in let mini = min il1 il4
        in sprawdz_brak (wartosc_od_do (norm_zero mini) (norm_zero maxi))

(* Mnoży wartość określoną przez przedział możliwych wartości przez*)
(* wartość określoną przez przedział brakujących wartości (w tej kolejności). *)
let razy_niebrak_brak war1 war2 =
        let (war3, war4) = rozbij_brak war2
        in let il1 = razy_niebrak war1 war3
        in let il2 = razy_niebrak war1 war4
        in suma_wartosci_nieskonczonych il1 il2

let razy war1 war2 =
        if czy_pusty war1 || czy_pusty war2 then pusty_przedzial
        else if war1 = zerowy_przedzial || war2 = zerowy_przedzial then zerowy_przedzial
        else
            let (br1, br2) = (brak war1, brak war2)
            in 
                 if not br1 && not br2 then razy_niebrak war1 war2
                 else if br1 && br2 then razy_brak war1 war2
                 else if not br1 && br2 then razy_niebrak_brak war1 war2
                 else razy_niebrak_brak war2 war1

let podzielic war1 war2 = razy war1 (odwrotnosc war2)
