let row_to_string (row : int list) =
  let delimiter = "|" in
  let row = List.fold_left
    (fun accumulator number ->
       let mark =
         if number = -1 then " x "
         else if number = 1 then " O "
         else "   " in
       let mark_with_delimiter = mark ^ delimiter
       in
       accumulator ^ mark_with_delimiter
    )
    delimiter
    row
  in
  row ^ "\n"

let generate_row_separator row =
  let row_length = String.length row in
  let row_delimiter = String.make row_length '-' in
  row_delimiter ^ "\n"

let print_playing_field playing_field =
  List.iter
    (fun row ->
       let row_string = row_to_string row in
       let row_delimiter = generate_row_separator row_string in
       print_string row_string;
       print_string row_delimiter;
    )
    playing_field
