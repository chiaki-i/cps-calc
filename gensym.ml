let counter = ref 0

let gensym (var_name : string) : string =
  let current_counter = !counter in
  counter := current_counter + 1;
  var_name ^ "_" ^ (string_of_int current_counter)
