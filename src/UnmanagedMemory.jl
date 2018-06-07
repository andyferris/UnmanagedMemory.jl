module UnmanagedMemory

import Base: getindex, setindex!, size, IndexStyle
import Base.Libc: free
using Base: IndexLinear, unsafe_convert
using Base.Libc: malloc

export UnmanagedArray, @unmanaged

#=
# Example
@unmanaged mutable struct A{T <: Real} <: B{T}
	a::T
end

# transforms to
abstract type B{T}; end

struct _A{T} # Mostly for `sizeof` and `fieldoffsets`
	a::T
end

struct A{T <: Real} <: B{T}
	ptr::Ptr{_A{T}}
end

function A{T}(::UndefInitializer) where {T <: Real}
	ptr = Base.Libc.malloc(sizeof(_A{T})) 
	if ptr === Ptr{Cvoid}(0)
		error("Memory allocation error")
	end
	A{T}(Base.unsafe_convert(Ptr{_A{T}}, ptr))
end

@inline function Base.getproperty(in::A{T}, field::Symbol) where {T <: Real}
	if field === :a
		if isbitstype(T)
			offset = fieldoffset(_A{T}, 1)
		    ptr = Base.unsafe_convert(Ptr{T}, getfield(in, :ptr) + offset)
		    return unsafe_load(ptr)
		end
	    error("Cannot use managed memory as a field type of an unmanaged mutable struct")
	end
	error("Type A{$T} does not have field $field")
end

@inline function Base.setproperty!(in::A{T}, field::Symbol, value) where {T <: Real}
	setproperty!(in, convert(T, value), field)
end

@inline function Base.setproperty!(in::A{T}, field::Symbol, value::T) where {T <: Real}
	if field === :a
		offset = fieldoffset(_A{T}, 1)
		if isbitstype(T)
		    ptr = Base.unsafe_convert(Ptr{T}, getfield(in, :ptr) + offset)
		    return unsafe_store!(ptr, value)
	    end
	    error("Cannot use managed memory as a field type of an unmanaged mutable struct")
	end
	error("Type A{$T} does not have field $field")
end

function Base.Libc.free(in::A{T})
    Base.Libc.free(getfield(in, :ptr))
end

function Base.show(io::IO, in::A{T}) where {T}
    print(io::IO, "A{$T}($(in.a))")
end
=#


# We can create @unmanaged mutable structs
"""
    @unmanaged ...

The `@unmanaged` macro creates something akin to a `mutable struct` with manual memory
management. Convenient for interacting with pointers to structs passed from other languages.
E.g.

```julia
@unmanaged mutable struct A <: SuperType  # note that A cannot have type parameters
    x::Int    # Fields must satisfy `isbitstype`
    y::Bool
end
```
"""
macro unmanaged(expr::Expr)
	@assert expr.head == :struct
    @assert length(expr.args) == 3
	@assert expr.args[1] == true

	name_expr = expr.args[2]
    if name_expr isa Symbol
    	name = name_expr
    	super_type = Any # This is never going to be `isbitstype`, but that seems like a concern for later
    elseif name_expr isa Expr
    	if name_expr.head == :(<:)
    		if name_expr.args[1] isa Symbol
    			name = name_expr.args[1]
    			super_type = name_expr.args[2]
    		elseif name_expr.args[1] isa Expr && name_expr.args[1].head == :curly
    			error("Generic types are not supported")
    		else
    			error("Problem parsing expression")
    		end
    	elseif name_expr.head == :curly
    		error("Generic types are not supported")
    	else
    		error("Problem parsing expression")
    	end
    else
    	error("Problem parsing expression")
    end
    internalname = Symbol("_" * string(name))
	fieldblock = expr.args[3]
	@assert fieldblock isa Expr && fieldblock.head == :block

    field_names = []
    field_types = []
	for field_expr in fieldblock.args
		if field_expr isa LineNumberNode
			continue
		end

		if field_expr isa Symbol
            push!(field_names, field_expr)
            push!(field_types, Any)
		elseif field_expr isa Expr && field_expr.head == :(::)
			@assert length(field_expr.args) == 2
            push!(field_names, field_expr.args[1])
            push!(field_types, field_expr.args[2])
        else
        	error("Problem parsing expression")
        end
	end

    # We create an *immutable* struct with these fields
	internal_struct_expr = Expr(:struct, false, internalname, fieldblock)
	external_struct_expr = Expr(:struct, false, :($name <: $super_type), :(begin; ptr::Ptr{$internalname}; end))
	bitstype_assertions = Expr(:block, [:(@assert isbitstype($T)) for T in field_types]...)
	getproperty_expr = Expr(:block, [:(if field === $(Expr(:quote, field_names[i]))
            offset = fieldoffset($internalname, $i)
            ptr = Base.unsafe_convert(Ptr{$(field_types[i])}, getfield(in, :ptr) + offset)
            return unsafe_load(ptr)
		end) for i in 1:length(field_names)]...)
	setproperty_expr = Expr(:block, [:(if field === $(Expr(:quote, field_names[i]))
            offset = fieldoffset($internalname, $i)
            ptr = Base.unsafe_convert(Ptr{$(field_types[i])}, getfield(in, :ptr) + offset)
            return unsafe_store!(ptr, convert($(field_types[i]), value))
		end) for i in 1:length(field_names)]...)

	return esc(quote
		$bitstype_assertions
		$internal_struct_expr
		$external_struct_expr

		function $name(::UndefInitializer)
			ptr = Base.Libc.malloc(sizeof($internalname)) 
	        if ptr === Ptr{Cvoid}(0)
		        error("Memory allocation error")
	        end
	        $name(Base.unsafe_convert(Ptr{$internalname}, ptr))
        end

		@inline function Base.getproperty(in::$name, field::Symbol)
			$getproperty_expr
			error("Type " * $(string(name)) * " does not have field $field")
		end

		@inline function Base.setproperty!(in::$name, field::Symbol, value)
			$setproperty_expr
			error("Type " * $(string(name)) * " does not have field $field")
		end

        function Base.Libc.free(x::$name)
             Base.Libc.free(getfield(x, :ptr))
        end

        function Base.show(io::IO, in::$name)
            print(io::IO, string($name) * "( )") # TODO!!
        end
	end)
end


# We can create UnmanagedArrays
"""
    UnmanagedArray(pointer, size)

Wrap `pointer::Ptr{T}` in an `AbstractArray` interface with the given dimensions. Useful
for arrays passed from C libraries.
"""
struct UnmanagedArray{T, N} <: AbstractArray{T, N}
	ptr::Ptr{T}
	size::NTuple{N, Int}
end
UnmanagedArray(ptr::Ptr{T}, s::NTuple{N, Integer}) where {T,N} = UnmanagedArray{T, N}(ptr, s)

"""
    UnmanagedArray{T}(undef, size)

Construct an `AbstractArray` of the given `size` with memory allocated directly via
`malloc`. This memory should be manually cleaned up by the user via `free`.
"""
UnmanagedArray{T}(::UndefInitializer, s::NTuple{N, Integer}) where {T, N} = UnmanagedArray{T,N}(undef, s)
UnmanagedArray{T,N}(::UndefInitializer, s::NTuple{N, Integer}) where {T, N} = UnmanagedArray{T,N}(convert(NTuple{N, Int}, s))
function UnmanagedArray{T,N}(::UndefInitializer, s::NTuple{N, Int}) where {T, N}
	ptr = malloc(sizeof(T) * prod(s))
	if ptr === Ptr{Cvoid}(0)
		error("Memory allocation error")
	end
	UnmanagedArray{T,N}(unsafe_convert(Ptr{T}, ptr), s)
end

IndexStyle(::Type{<:UnmanagedArray}) = IndexLinear()

size(a::UnmanagedArray) = a.size

@inline function getindex(a::UnmanagedArray, i::Integer)
	@boundscheck if i < 1 || i > prod(a.size)
	    throw(BoundsError(a, i))
	end

	if isbitstype(eltype(a))
		return unsafe_load(a.ptr, i)
	else
		error("Cannot use managed memory as the element type of an unmanaged array")
	end
end

@inline function setindex!(a::UnmanagedArray{T}, v::T, i::Integer) where {T}
	@boundscheck if i < 1 || i > prod(a.size)
	    throw(BoundsError(a, i))
	end

    if isbitstype(eltype(a))
	    unsafe_store!(a.ptr, v, i)
	    return a
	else
		error("Cannot use managed memory as the element type of an unmanaged array")
	end
end

free(a::UnmanagedArray) = free(a.ptr)

end # module
