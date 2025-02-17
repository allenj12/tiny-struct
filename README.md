## Tiny Struct
Represents a struct as a chez scheme fixnum, this can have performance tradeoffs when comparing to do something similar with a bytevector. Tiny Structs must be fixnum bit length - 1 (Tiny Struct does not currently take advantage of the sign bit). Here are the supported types:

```
(define-tiny-struct my-struct
      u 8 my-unsigned-int ;; 8 can be any number less than the bit width of a fixnum
      s 20 my-signed-int
      char c ;; 8 bit representation that gets converted for you on set/get
      bool b ;; 1 bit representation that gets converted for you on set/get
      array u 4 5 my-array ;; an 'array' of 4 bit unsigned ints of length 5. An array can contain any prior type 
      )
```

## Examples
Once created get and set functions are created for each field, they technically can work with any fixnum and act immutably. The following takes the before struct and just sets the char and the boolean, and retrieves the boolean out of the struct

```
(my-struct-b (my-struct-c-set (my-struct-b-set 0 #t) #\A)) 
```

Arrays have the same naming pattern but take an additional index paramter. Below we set the 4th (0-indexed) element of the array to 15, and retrieve it after setting other fields.

```
(my-struct-my-array (my-struct-c-set (my-struct-my-array-set 0 4 15) #\A) 4)
```
## Performance
### Pros
Tiny struct from my own testing seems to be more performant than byte arrays in the following scenarios

1. When the first field in the struct is accessed primarily
2. You want an unconventional size, like a u24 int
3. You operate on multiple fields of the struct before mutating the fxvector

### Cons
Byte arrays from my own testing seem to be more performant than Tiny Struct when:

1. You are primarily accessing the middle field of the "struct"
2. You only are working on one field at a time.
