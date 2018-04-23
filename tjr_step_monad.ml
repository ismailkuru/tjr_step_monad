(* a simple step monad ---------------------------------------------- *)

(* NOTE the monad in isa_btree is there so that we can be explicit
   about errors; it is NOT there to model steps (the definitions are all
   small step anyway). *)


(* FIXME do we also want a separate package for type either? this
   seems a bit complicated *)
type ('a,'b) either = [ `Inl of 'a | `Inr of 'b ] 


(* We want to reduce the interface as much as possible, but allow
   extra functionality when absolutely needed. *)

module type MONAD = sig
  type ('a,'w) m
  val return: 'a -> ('a,'w) m
  val bind: ('a -> ('b,'w)m) -> ('a,'w)m -> ('b,'w)m
  val fmap: ('a -> 'b) -> ('a, 'w) m -> ('b, 'w) m
end

(* NOTE the following should not be accessed directly - we want to
   hide the implementation types *)
module Step_monad_implementation = struct

  type ('a,'w) step_monad = Step of ('w -> 'w * ('a,('a,'w)step_monad) either)

  type ('a,'w)k_ = ('a,('a,'w)step_monad) either

  type ('a,'w)m = ('a,'w)step_monad

  let dest_Step (Step f) = f

  let return a = Step(fun w -> (w,`Inl a))

  let rec bind : ('a,'w)m -> ('a -> ('b,'w)m) -> ('b,'w)m = 
    fun a b ->
      Step(fun w ->
          dest_Step a |> fun a ->
          a w |> fun (w',rest) ->
          (w',
           match rest with
           | `Inl a -> `Inr(b a)
           | `Inr a -> `Inr(bind a b)))

  let _ = bind

  (* prefer "reversed" version of bind *)
  let bind ab a = bind a ab
  

  (* FIXME in MONAD? *)
  let rec fmap f a = 
    a |> dest_Step |> fun a ->
    Step(fun w -> 
        a w |> fun (w,rest) ->
        (w,
         match rest with
         | `Inl a -> `Inl (f a)
         | `Inr a -> `Inr (fmap f a)))

  let _ = fmap



  let get_world () = Step(fun w -> (w,`Inl w))
      
  let _ = get_world

  let set_world w = Step(fun w' -> (w,`Inl ()))

  let _ = set_world

end

(* FIXME note ocamldoc doesn't seem to get the type of this module correct *)
module Step_monad = 
  (Step_monad_implementation : MONAD with type ('a,'w) m = ('a,'w) Step_monad_implementation.m)


(* FIXME why not just lift a? *)
(*
let with_state 
    (type a b w) (a:w -> a*w) (b:a->(b,w)step_monad) : (b,w) step_monad
  =
  let a : (a,w)step_monad = Step(fun w -> a w |> fun (a,w) -> (w,Inl a)) in
  bind a b
*)

include Step_monad



(* extra ops -------------------------------------------------------- *)

module Extra = struct

  open Step_monad_implementation

  let with_state (type a b w) (a:w -> a*w) : (a,w)m = 
    Step(fun w -> a w |> fun (a,w) -> (w,`Inl a))


  (* basic version - runs till completion *)
  let rec run w a = 
    dest_Step a |> fun a ->
    a w |> fun (w,rest) ->
    match rest with 
    | `Inl a -> (w,a)
    | `Inr a -> run w a

  let _ = run



  (* NOTE for "unexpected" errors, for which there is no obvious way
     to continue, we halt and expect some "higher layer" to pick up
     the pieces; for this we need some way to check the state to
     detect whether there is an unexpected error... typically there
     will be a single optional reference in the state which, if set,
     describes the error that occurred
  *)
  (* used to be called run ~dest_exceptional *)
  let run_with_halt ~halt w a =
    let rec run w a = 
      match halt w with
      (* FIXME shouldn't discard Some info? or make a boolean *)
      | true -> w, Error `Attempt_to_step_halted_state
      | false -> 
        dest_Step a |> fun a ->
        a w |> fun (w,rest) -> 
        match rest with 
        | `Inl a -> w, Ok a
        | `Inr a -> run w a
    in
    run w a

  let _ = run_with_halt

end
