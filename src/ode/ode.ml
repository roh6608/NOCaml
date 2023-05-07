(* euler method for solving ode *)

let ode_solve_euler f x0 y0 xend h =
	 let vals = seq x0 xend h in
	 let euler (x,y) x' = 
	 	let h = x' -. x in
		(x', y +. h *. (f (x +. h) y)) in
	 scan_left euler (x0,y0) vals
