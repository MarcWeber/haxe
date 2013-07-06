let first() =
  let sum = fun a b -> a + b
  in (
  let m_i = function
  | 1 -> print_string ((string_of_int 1) ^ "\n");
  | 2 -> if  (sum 2 3 == 5) then print_string "yes" else print_string "no";
  | _ -> print_string "other\n";
  in (
  print_string (string_of_int (sum 2 3) ^ "\n");
  m_i 2;
  print_string "first\n";
  ))
  
in

first();;
print_string "A\n";;

print_string "B\n";
