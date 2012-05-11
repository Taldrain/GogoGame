(** This module provides an efficient, imperative implementation of Fibonacci
    heaps based on the pseudocode from CLRS, 2nd ed., ch. 20.

    {3 Fiblib }

    Fibonacci heaps are kind of an obscure data structure, but they have their
    purposes. Operations that do not delete an element run in [O(1)] amortised
    time. In particular, if the number of [extract-min] and [delete] operations is
    small compared to the number of other operations, fibonacci heaps are a win
    (CLRS 477).

    In this library, each heap is conceptually a min-heap (elements are closer
    to the root whose keys are less than other keys). However, the comparison
    function is parameterised by functorisation, so it is just as easy to create a
    min-heap as a max-heap.

    In particular, if your keys are integers, and the comparison function is
    {!Pervasives.compare}, you will get a min-heap, e.g., data with key 5 will come
    out of the heap before data with key 10.
*)


(** The signature of the output of {!Make}. *)
module type S = sig


  (** {3 Types } *)

  type key
      (** The totally-ordered key type.

          The comparison of this key determines whether this is a min-heap or a
          max-heap. If [fibkey] is [int], for example, and is compared by
          [Pervasives.compare], you get a min-heap.

          In general, if [f] compares keys, it is required that that [f k k' <
          0] if and only if [k] should occur before [k'] in extraction order. *)

  type 'a fibheap
      (** A fibonacci heap containing elements of type ['a]. *)

  type 'a fibnode
      (** A fibonacci heap node. *)

  (** {3 Exceptions } *)


  exception Empty;;
  (** Thrown when the heap is empty. *)

  exception Key_too_big;;
  (** Thrown when the new key given for a call to {!fibheap_decrease_key}
      would not result in a decreased key. *)

  (** {3 Operations } *)

  val fibheap_create : unit -> 'a fibheap
    (** Create a fibonacci heap.

        @return a fresh heap *)

  val fibheap_insert : 'a fibheap -> 'a fibnode -> unit
    (** [fibheap_insert heap node] inserts [node] into [heap]. *)

  val fibheap_insert_data : 'a fibheap -> 'a -> key -> 'a fibnode
    (** [fibheap_insert_data heap data key] implicitly creates a {!fibnode} and
        inserts it into the heap.

        @return the created {!fibnode} *)

  val fibheap_extract_min : 'a fibheap -> 'a fibnode
    (** [fibheap_extract_min heap] extracts the node with the minimum key from
        [heap].

        @return the {!fibnode} whose {!key} is minimum in [heap] *)

  val fibheap_extract_min_data : 'a fibheap -> 'a
    (** [fibheap_extract_min heap] extracts the node with the minimum key from
        [heap].

        @return the data whose {!key} is minimum in [heap] *)

  val fibheap_delete : 'a fibheap -> 'a fibnode -> unit

  val fibheap_decrease_key : 'a fibheap -> 'a fibnode -> key -> unit
    (** [fibheap_decrease_key heap node new_key] decreases the value of the key
        paired with [node] in the [heap].

        @raise Not_found if [node] is not in the heap
        @raise Key_too_big if [node]'s key is smaller than [new_key] *)

  val fibheap_size : 'a fibheap -> int
    (** [fibheap_size heap].

        @return the number of elements in [heap]. *)


  (** {3 Constructors and Accessors } *)

  val fibnode_new : key:key -> data:'a -> 'a fibnode
    (** Create a {!fibnode} by pairing a key with some data. *)

  val fibnode_data : 'a fibnode -> 'a

  val fibnode_key : 'a fibnode -> key

  (** {3 Printing } *)

  val fibheap_print : ('a -> string) -> Format.formatter -> 'a fibheap -> unit
    (** [fibheap_print to_string formatter heap] pretty-prints the heap to
        [formatter] using [to_string] to print each data element. *)

end

(** The type of key comparators.

    You need to define:

    - [t], the type of your keys; and
    - [compare : t -> t -> int], an ordering for your keys. See {!S.key} for ordering details.
    - [min], a value such that for any value [x] of type [t], [compare min x <= 0]. *)
module type KeyOrderType = sig
  include Map.OrderedType (* declares type t *)

  val min : t
end

(** Creates a Fibonacci heap module with key comparison done by [Ord]. *)
module Make (Ord : KeyOrderType) : S with type key = Ord.t

(* this comment intentionally contentless *)
