let print_list f l =
  print_char '[';
  List.iter
    (fun x ->
      f x;
      print_char ',')
    l;
  print_char ']'
