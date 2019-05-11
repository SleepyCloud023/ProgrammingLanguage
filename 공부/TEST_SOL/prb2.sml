signature S =
sig
  val mylist : 'a list
end

structure M :> S =
struct
  val mylist = []
end


