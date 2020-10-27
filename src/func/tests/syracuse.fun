let even n = (n mod 2) = 0 in
let rec syracuse n = (n, if n = 1 then 1 else if even n then syracuse (n/2) else syracuse (3*n+1)) in
syracuse 69