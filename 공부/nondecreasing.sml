fun nondecreasing xs = 
  cas xs of 
     [] => true
  | x::[] => true
  | x::y::
