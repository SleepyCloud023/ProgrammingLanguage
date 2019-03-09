(* This is a comment. My first ML program *)

val x = 34;
(* static environ: x:int *)
(* dynamic environ: x->34 *)
val y = 17;

val z = (x + y) + (y + 2);

val q = z + 1;

val abs_of_z = if z<0 then 0 - z else z;

val abs_of_z_simpler = abs z;