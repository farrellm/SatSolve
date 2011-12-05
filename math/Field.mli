module type FIELD = 
  sig
    type field
    val plus: field -> field -> field
    val negative: field -> field
    val zero: field
    val is_zero: field -> bool

    val multiply: field -> field -> field
    val inverse: field -> field
    val one: field

    val print: field -> unit
  end
      
