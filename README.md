# UnmanagedMemory.jl

This package provides two basic helpers for dealing with unmanaged memory, for example
structs and arrays coming from alien C code.

`UnmanagedArray` is a simple `AbstractArray` that wraps a pointer. It's element type must
be concrete and `isbitstype`.

The `@unmanaged` macro helps one create something akin to a mutable struct, which is really
just a pointer with `getproperty` and `setproperty!` overloads.

```
@unmanaged mutable struct A <: SuperType
    x::Int
    y::Bool
end
```
The fields must all be concrete `isbitstype`s, and the type itself cannot be generic. These
are similar limitations to C.