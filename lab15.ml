(*
                             CS51 Lab 15
         Lazy Programming and Infinite Data Structures Part 2
 *)
(*
                               SOLUTION
 *)

(* In lab 14 you became familiar with the explicit implementation of
lazy streams. In this lab you will delve further into lazy evaluation
and look at how laziness is implemented natively in OCaml with the
Lazy module, which we use to implement a NativeLazyStreams module. *)

(*====================================================================
Part 1: Using OCaml's Lazy module

All of the recomputation going on behind the scenes with these
stream-based solutions is prohibitive. Chapter 17 of the textbook
describes the use of *memoizing* to eliminate the recomputation, and
showed an implementation in terms of refs. That functionality is
actually already available in OCaml through its Lazy module. The Lazy
module introduces a new polymorphic type -- 'a Lazy.t -- of delayed
values of type 'a, and a new function Lazy.force : 'a Lazy.t -> 'a
that forces a delayed computation to occur, saving the result if this
is the first time the value was forced and simply returning the saved
value on later requests. For instance, suppose we've defined the
Fibonacci function "eagerly" as: *)

let rec fib (n : int) : int =
  if n < 2 then n
  else (fib (n - 1)) + (fib (n - 2)) ;;

(* Then a delayed computation of the 42nd Fibonacci number would be *)

let fib42 : int Lazy.t =
  lazy (fib 42) ;;

(* Here, we force the computation twice in a row, timing the two calls:

# CS51.call_reporting_time Lazy.force fib42 ;;
Elapsed time: 13.380860
- : int = 267914296
# CS51.call_reporting_time Lazy.force fib42 ;;
Elapsed time: 0.000000
- : int = 267914296

The first time through takes 13 seconds, the second less than a
microsecond. *)

(*....................................................................
Exercise 1. The NativeLazyStreams module, found in the file
nativeLazyStreams.ml, is an incomplete reimplementation of the
LazyStreams module from Lab 14, but using OCaml's native Lazy
module. Complete this implementation by implementing smap, smap2, and
sfilter in that file. You may want to refer to Lab 14, especially the
LazyStreams module provided there and its Exercise 6.
....................................................................*)

(* Now we can redo the Fibonacci example. First, we open the
NativeLazyStreams module so we can use its bits more easily. *)

open NativeLazyStreams ;;


    (* Digression: We've just opened NativeLazyStreams above, so we can
    make use of its elements in this file. But if you want to use it in
    the REPL, you'll need to make it accessible there as well, e.g.,

        # #mod_use "nativeLazyStreams.ml" ;;
        # open NativeLazyStreams ;;
     *)

(* We implement the Fibonacci sequence as a
NativeLazystreams.stream *)

let rec fibs =
  lazy (Cons(0, lazy (Cons(1, smap2 (+) fibs (tail fibs))))) ;;

(* We run it twice, generating the first 50 Fibonacci numbers:

    # CS51.call_reporting_time (first 50) fibs ;;
    time (msecs): 0.029087
    - : int list =
    [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597;
     2584; 4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
     514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465; 14930352;
     24157817; 39088169; 63245986; 102334155; 165580141; 267914296; 433494437;
     701408733; 1134903170; 1836311903; 2971215073; 4807526976; 7778742049]

    # CS51.call_reporting_time (first 50) fibs ;;
    time (msecs): 0.006914
    - : int list =
    [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597;
     2584; 4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
     514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465; 14930352;
     24157817; 39088169; 63245986; 102334155; 165580141; 267914296; 433494437;
     701408733; 1134903170; 1836311903; 2971215073; 4807526976; 7778742049]

This version is much faster, even the first time around. Why? *)

(*....................................................................
Exercise 2. As practice in using NativeLazyStreams, implement a
function geo : float -> float -> float stream that returns a geometric
series as an infinite stream, where the first argument is the initial
value in the stream, and each successive value is multiplied by the
second argument. For example:

    # first 10 (geo 0.5 0.5) ;;
    - : float list =
    [0.5; 0.25; 0.125; 0.0625; 0.03125; 0.015625; 0.0078125; 0.00390625;
     0.001953125; 0.0009765625]
    # first 10 (geo 1. 2.) ;;
    - : float list = [1.; 2.; 4.; 8.; 16.; 32.; 64.; 128.; 256.; 512.]
....................................................................*)

let rec geo init mult =
  lazy (Cons (init, (geo (init *. mult) mult))) ;;

(*====================================================================
Part 2. Eratosthenes' sieve revisited

We return to the Eratosthenes' sieve example from the last lab. For
reference, here are the implementations of nats, sieve, and primes
from that lab, which used the LazyStreams module.

    let rec nats =
      fun () -> Cons(0, smap ((+) 1) nats) ;;

    let not_div_by (n : int) (m : int) : bool =
      not (m mod n = 0) ;;

    let rec sieve (s : int stream) : int stream =
      let Cons(h, t) = s () in
      fun () -> Cons(h, sieve (sfilter (not_div_by h) t)) ;;

    let primes : int stream = sieve (tail (tail nats)) ;;
 *)

(*....................................................................
Exercise 3. Redo the Eratosthenes sieve using the NativeLazyStreams
module by completing the values and functions below.
....................................................................*)

let rec nats =
  lazy (Cons(0, smap ((+) 1) nats)) ;;

let not_div_by (n : int) (m : int) : bool =
  not (m mod n = 0) ;;

let rec sieve s =
  lazy (Cons(head s, sieve (sfilter (not_div_by (head s)) (tail s)))) ;;

let primes = sieve (tail (tail nats)) ;;

(*....................................................................
Exercise 4. How much further can you get computing primes now that the
recomputation problem is solved?  Implement a function to find the nth
element in a stream, and use it to find out the 2000th prime.
....................................................................*)

let rec nth (s : 'a stream) (n : int) : 'a =
  if n = 0 then head s
  else nth (tail s) (n - 1) ;;

nth primes 2000 ;;


(* Here's the same table from last lab, now with the new sieve: *)
exception Done ;;

let prime_timing () =
  try
    print_endline "Testing sieve based on native lazy streams";
    for n = 1 to 100 do
      let _l, t = CS51.call_timed (first n) primes in
      Printf.printf "%3d -- %12.8f\n" n t;
      if t > 0.5 then raise Done
    done
  with Done -> () ;;

(*====================================================================
Part 3: Series acceleration with infinite streams

In the Pi module (see the file pi.ml), we provide the definitions of
lazy streams using OCaml's native Lazy module, up to and including
code for approximating pi through partial sums of the terms in a
Taylor series. In the next problem, you'll use streams to find faster
approximations for pi.

Recall from the reading the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by **averaging
adjacent elements in the approximation stream**. *)

(*....................................................................
Exercise 5: Implementing average on streams

Write a function average that takes a float stream and returns another
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
....................................................................*)

let average (s : float stream) : float stream =
  smap2 (fun x y -> (x +. y) /. 2.0) s (tail s) ;;

(* Now instead of using the stream of approximations in pi_sums, you
can instead use the stream of averaged pi_sums, which converges much
more quickly. Test that it requires far fewer steps to get within,
say, 0.001 of pi. You'll want to record your results below for
Exercise 6. *)

(*....................................................................
Exercise 6: Implementing Aitken's method

An even better accelerator of convergence for series of this sort is
Aitken's method.

Given a stream s_1, s_2, s_3, etc., instead of averaging contiguous
elements, that is, generating the accelerated stream of elements s'_1
s'_2, s'_3, etc. where

    s'n = ( s_n + s_(n-1) ) / 2     ,

Aitken's method uses an alternative formula for s'n, as given in the
textbook in Section H.3.1, and reproduced at
http://url.cs51.io/aitken.

Write a function to apply this accelerator to a stream, and use it to
generate approximations of pi.
....................................................................*)

(* Here are two equivalent definitions. *)
let aitken (s: float stream) : float stream =
  smap2 (-.)
        s
        (smap2 (/.)
               (smap (fun x -> x *. x)
                     (smap2 (-.) s (tail s)))
               (smap2 (+.)
                      (smap2 (-.)
                             s
                             (smap (( *. ) 2.) (tail s)))
                      (tail  (tail s)))) ;;

let aitken2 (s: float stream) : float stream =
  smap2 (-.)
        (tail (tail s))
        (smap2 (/.)
               (smap (fun x -> x *. x)
                     (smap2 (-.) (tail s) (tail (tail s))))
               (smap2 (+.)
                      (smap2 (-.)
                             (tail (tail s))
                             (smap (( *. ) 2.) (tail s)))
                      s)) ;;

(*......................................................................
Exercise 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method
    ---------------------------------------------------------
    0.1      |           |                   |
    ---------------------------------------------------------
    0.01     |           |                   |
    ---------------------------------------------------------
    0.001    |           |                   |
    ---------------------------------------------------------
    0.0001   |           |                   |
    ---------------------------------------------------------
......................................................................*)
