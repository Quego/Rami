let play = 
  while true do
    print_string "Pour jouer au Rami entrez 1 , pour jouer au Rummikub entrez 2\n";
    let x = read_int() in
    if (x==1) then print_string "Vous jouez au Rami\n"
    else print_string "Vous jouez au Rummikub\n";
  done
;;


