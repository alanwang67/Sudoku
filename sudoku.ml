let indexMatrix g i j = 
  List.nth (List.nth g i) j 

let rec compareList l1 l2 =
  match l1,l2 with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 when h1 = h2 -> compareList t1 t2
  | _, _ -> false
    
let rec compareMatrix g1 g2 =
  match g1,g2 with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 when compareList h1 h2 -> compareMatrix t1 t2
  | _, _ -> false

let rec getColumn grid j =
  match grid with
  | [] -> [] 
  | hd :: tl -> (List.nth hd j) :: getColumn tl j 

let getSubGrid grid i j : int list =
  let i = 3 * (i / 3) in
  let j = 3 * (j / 3) in
  let listOfRows = (List.filteri (fun xi ei -> xi >= i && xi < (i+3)) grid) in
  let rec helper lst j =
    match lst with
    | [] -> []
    | hd :: tl -> (List.filteri (fun yj ej -> yj >= j && yj < (j+3)) hd) @ (helper tl j)
  in
  helper listOfRows j

let isValid grid i j v =
  let rowIsValid grid i v =
    not (List.exists (fun e -> e = v) (List.nth grid i))
  in
  let columnIsValid grid j v =
    not (List.exists (fun e -> e = v) (getColumn grid j))
  in
  let subGridIsValid grid i j v =
    not (List.exists (fun e -> e = v) (getSubGrid grid i j))
  in
  (rowIsValid grid i v) && (columnIsValid grid j v) && (subGridIsValid grid i j v)

let rec printGrid grid =
  match grid with 
  | [] -> ()
  | hd :: tl ->  List.iter (Printf.printf "%d ") hd ; printGrid tl

let rec editGrid grid i j v =
  match grid with
  | [] -> []
  | hd :: tl -> if i = 0 then (List.mapi (fun i e -> if i = j then v else e) hd) :: tl else (hd :: editGrid tl (i - 1) j v)

let solve grid =
  let getNextValue i j =
    if j >= 8 then (i + 1,0) else (i,j + 1)
  in 
  let rec helper grid i j = 
    if i >= 9 then (true, grid) else
    (let values = [1; 2; 3; 4; 5; 6; 7; 8; 9]
     in
     if (indexMatrix grid i j = 0) then
       List.fold_left (fun (b, g) e -> if b = true then (b, g) else
                                          (if (isValid grid i j e) then
                                            let (nextI, nextJ) = getNextValue i j in 
                                            helper (editGrid grid i j e) nextI nextJ 
                                           else (b, g)))
         (false, grid) values
     else
       let (nextI, nextJ) = getNextValue i j in 
       helper grid nextI nextJ)
  in
  let (b, g) = helper grid 0 0 in
  g


let _ = Format.print_bool (compareMatrix (solve [[5; 3; 0; 0; 7; 0; 0; 0; 0]; [6; 0; 0; 1; 9; 5; 0; 0; 0]; [0; 9; 8; 0; 0; 0; 0; 6; 0]; [8; 0; 0; 0; 6; 0; 0; 0; 3]; [4; 0; 0; 8; 0; 3; 0; 0; 1]; [7; 0; 0; 0; 2; 0; 0; 0; 6]; [0; 6; 0; 0; 0; 0; 2; 8; 0]; [0; 0; 0; 4; 1; 9; 0; 0; 5]; [0; 0; 0; 0; 8; 0; 0; 7; 9]]) [[5; 3; 4; 6; 7; 8; 9; 1; 2]; [6; 7; 2; 1; 9; 5; 3; 4; 8]; [1; 9; 8; 3; 4; 2; 5; 6; 7]; [8; 5; 9; 7; 6; 1; 4; 2; 3]; [4; 2; 6; 8; 5; 3; 7; 9; 1]; [7; 1; 3; 9; 2; 4; 8; 5; 6]; [9; 6; 1; 5; 3; 7; 2; 8; 4]; [2; 8; 7; 4; 1; 9; 6; 3; 5]; [3; 4; 5; 2; 8; 6; 1; 7; 9]])

let _ = printGrid (solve ([[5; 3; 0; 0; 7; 0; 0; 0; 0]; [6; 0; 0; 1; 9; 5; 0; 0; 0]; [0; 9; 8; 0; 0; 0; 0; 6; 0]; [8; 0; 0; 0; 6; 0; 0; 0; 3]; [4; 0; 0; 8; 0; 3; 0; 0; 1]; [7; 0; 0; 0; 2; 0; 0; 0; 6]; [0; 6; 0; 0; 0; 0; 2; 8; 0]; [0; 0; 0; 4; 1; 9; 0; 0; 5]; [0; 0; 0; 0; 8; 0; 0; 7; 9]]))

