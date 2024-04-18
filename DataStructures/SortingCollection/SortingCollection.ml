
module type SORTING_COLLECTION = 
sig
  type t
  type sortable

  val empty : sortable
  val add : t -> sortable -> sortable
  val sort : sortable -> t list 
end
