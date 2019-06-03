val map =
  fn f =>
    fn xs =>
      case of lst 
        [] => []
      | x::xs => f(x)::(map f xs)
