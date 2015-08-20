"""
## AbstractRaggedArray

The AbstractRaggedArray is a subtype of AbstractArray with four parameters:
* T: The element type
* N: The dimensionality
* RD: The ragged dimension
* OD: The number of dimensions after the ragged dimension (N-RD)

In addition to implementing the AbstractArray methods, AbstractRaggedArray
subtypes should implement `raggedlengths`. `Base.size` should return the maximum
extents of the array. They *must not* implement the Base `unsafe_get/setindex`
methods.
"""
abstract AbstractRaggedArray{T,N,RD,OD} <: AbstractArray{T,N}

"""
    raggedlengths(R, indexes...)

As a special extension to size, `AbstractRaggedArray`s must allow looking up
the ragged lengths along the ragged dimension by providing context in the form
of the trailing `indexes` for the outer dimensions. The indexes need not index
at the full dimensionality of the array. The indexes may be non-scalar, in
which case a corresponding array of ragged lengths is returned.

All `AbstractRaggedArray`s must implement this method.
"""
function raggedlengths end

@generated function raggedsize{T,N,RD}(R::AbstractRaggedArray{T,N,RD})
    Expr(:tuple, [:(size(R, $d)) for d=1:RD-1]..., raggedlengths(R, :), [:(size(R, $d)) for d=RD+1:N]...)
end

# similar without changing dimensions -- use the same ragged sizes, too!
Base.similar{T}(R::AbstractRaggedArray{T}) = similar(R, T)
Base.similar{T,N,RD,OD,S}(R::AbstractRaggedArray{T,N,RD,OD}, ::Type{S}) = similar(R, S, raggedsize(R))
# similar with a different ragged size
Base.similar(R::AbstractRaggedArray, I::Union{Int,Array{Int},Tuple}...) = similar(R, I)
Base.similar{T}(R::AbstractRaggedArray{T}, I::Tuple{Vararg{Union{Int, Array{Int}, Tuple}}}) = similar(R, T, I)
Base.similar{T}(R::AbstractRaggedArray, ::Type{T}, I::Union{Int,Array{Int},Tuple}...) = similar(R, T, I)
# similar with a non-ragged size becomes a regular Array
Base.similar{T}(R::AbstractRaggedArray, ::Type{T}, I::Tuple{Vararg{Int}}) = Array(T, I...)


import Base: _checkbounds, trailingsize, throw_boundserror
@inline Base.checkbounds(R::AbstractRaggedArray, i::AbstractVector{Bool}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::AbstractRaggedArray, i::AbstractArray{Bool}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::AbstractRaggedArray, i::Union{AbstractArray, Real, Colon}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::AbstractRaggedArray, i::Union{AbstractArray, Real, Colon}...) = checkbounds_impl(R, i...)

# Check both the ragged and rectangular bounds for an AbstractRaggedArray
@generated function checkbounds_impl{_,AN,RD}(A::AbstractRaggedArray{_,AN,RD}, I...)
    meta = Expr(:meta, :inline)
    N = length(I)
    N <= RD && return :(throw(ArgumentError("linear indexing through a ragged dimension is unsupported")))
    Isplat = [:(I[$d]) for d=1:N]
    error = :(throw_boundserror(A, tuple($(Isplat...))))
    outer_idxs = Expr[:(I[$d]) for d=RD+1:N]
    args = Expr[]
    for dim=1:N-1
        dim == RD && continue # Check the ragged dimension last
        push!(args, :(_checkbounds(size(A,$dim), I[$dim]) || $error))
    end
    push!(args, :(_checkbounds(trailingsize(A,Val{$N}), I[$N]) || $error))
    # Check the ragged dimension
    if all(i->i<:Real, I[RD+1:N])
        # With all-scalar outer indexes, just use them as a ragged length lookup
        push!(args, :(_checkbounds(raggedlengths(A, $(outer_idxs...)), I[$RD]) || $error))
    else
        # Otherwise, there may be different ragged lengths across the indexes
        push!(args, quote
            raglens = raggedlengths(A, $(outer_idxs...)) # An array of lengths
            for i in eachindex(raglens)
                _checkbounds(raglens[i], I[$RD]) || $error
            end
        end)
    end
    Expr(:block, meta, args...)
end
