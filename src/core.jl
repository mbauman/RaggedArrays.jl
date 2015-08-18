
immutable RaggedArray{T,N,RD,OD} <: AbstractArray{T,N}
    data::Vector{T}
    square_size::NTuple{N,Int}    # maximum rectangular extents
    ragged_lengths::Array{Int,OD} # OD = N-RD; the shape of the outer dimensions
    ragged_offsets::Array{Int,OD}
end

# TODO: perhaps allow not specifying the outer dimensions, inferring them from
# the length and/or size of the ragged lengths.
"""
    RaggedArray(T, sizes...)

Construct a RaggedArray with eltype T and given sizes. One dimension must be an
array or tuple (or other iterable collection) of ragged lengths that vary over
all the outer dimensions. The last dimension may not be ragged.
"""
@generated function RaggedArray{T}(::Type{T}, szs...)
    N = length(szs)
    RD = 0
    for i = 1:N
        szs[i] <: Int && continue
        if RD == 0
            RD = i
        else
            return :(throw(ArgumentError("more than one ragged dimension given")))
        end
    end
    RD == 0 && return :(throw(ArgumentError("no ragged dimension given; use a normal Array instead")))
    N == RD && return :(throw(ArgumentError("ragged dimension must not be the last dimension given")))
    I = Expr(:tuple, [:(szs[$d]) for d=1:RD-1]...)
    O = Expr(:tuple, [:(szs[$d]) for d=RD+1:N]...)
    S = Expr(:tuple, I.args..., :(maximum(szs[$RD])), O.args...)
    quote
        square_size = $S
        inner_size = $I
        outer_size = $O
        # TODO: allow broadcasting of the ragged size array?
        # TODO: give a better error for wrong size ragged length array
        ragged_lengths = reshape(collect(Int, szs[$RD]), outer_size)
        ragged_offsets = cumsum0(ragged_lengths)
        len = prod(inner_size) * (ragged_offsets[end] + ragged_lengths[end])
        data = Vector{$T}(len)
        RaggedArray{$T,$N,$RD,$(N-RD)}(data, square_size, ragged_lengths, ragged_offsets)
    end
end

"internal helper function to compute `cumsum`, but starting at 0. No need to be
general here; just support Array{Int} and don't worry about types or eachindex."
function cumsum0(A::Array{Int})
    C = similar(A)
    C[1] = 0
    for i=2:length(A)
        C[i] = C[i-1] + A[i-1]
    end
    C
end
# Size is the toughest part in determining how to best define a ragged array.
# There are several options here:
#    1. Define size as the maximum "outermost" extents. Accessing a ragged
#       dimension out of bounds throws a bounds error, even though it's within
#       the rectangular size. Tricky parts:
#         * `eachindex` only wants to visit valid indexes, so at least one of
#           these equalities `prod(sum(R)) == length(R) == length(eachindex(R))`
#           must be false. I'm not sure which equality is the best to break.
#         * Linear indexing through a ragged dimension is ambiguous -- do you
#           want to visit all indices in the rectangle or just the valid ones?
#           Probably better to not support it at all to avoid silent errors.
#         * We must not listen to `@inbounds` since it may be applied from
#           library code that assumes everything in 1:size(A, d) is in bounds.
#       These may be mitigated by preventing linear indexing through the ragged
#       dimension, claiming to be LinearSlow, and aggressively checking bounds.
#    2. OR: Throw an error for `size(R)` and `size(R, d)`, where d is the ragged
#       dimension. This is ostensibly more correct, but it doesn't work as
#       easily in places where (1) does just fine. If we do this, I'm not sure
#       it's worth trying to be an AbstractArray without a fully valid size.
#    3. Or maybe some middle ground:
#         * They could behave like a NullableArray, returning Nullable{T}() for
#           out of bounds accesses, but that seems awkward and heavy-handed.
#         * Another idea was to have size return a special RaggedDimension type
#           that stored all the ragged lengths but behaved like an integer with
#           the maximum length, perhaps with very little arithmetic implemented.
#           This would be type-unstable, and it doesn't seem to gain us much.
# For now, it's nice to be able to piggy-back on AbstractArray with (1). So few
# AbstractArray methods work with (2) that it's probably not worth being a 
# subtype of AbstractArray if we go that route.

# (Implementation of #1)
Base.length(R::RaggedArray) = length(R.data) # == length(eachindex(R)) != prod(size(R))
Base.size(R::RaggedArray) = R.square_size
# (Implementation of #2)
# Base.length(R::RaggedArray) = length(R.data)
# Base.size(R::RaggedArray) = throw(ArgumentError("size of a ragged array is undefined"))
# @inline function Base.size{T,N,RD}(R::RaggedArray{T,N,RD}, d)
#     d == RD && throw(ArgumentError("size of the ragged dimension ($d) is undefined"))
#     d <= N ? R.square_size[d] : 1
# end
# "`rectsize(R)` is the maximum extent of the ragged array R"
# rectsize(R::RaggedArray) = R.square_size
# rectsize{_,N}(R::RaggedArray{_,N}, d) = d <= N ? R.square_size[d] : 1

# As a special extension to `size`, allow giving context to look up ragged length
@inline function Base.size{T,N,RD}(R::RaggedArray{T,N,RD}, dim, idxs::Tuple{Vararg{Int}})
    dim != RD && return (dim <= N ? R.square_size[dim] : 1)
    R.ragged_lengths[idxs...]
end

# similar without changing dimensions -- use the same ragged sizes, too!
Base.similar{T}(R::RaggedArray{T}) = similar(R, T)
Base.similar{T,N,RD,OD,S}(R::RaggedArray{T,N,RD,OD}, ::Type{S}) =
    RaggedArray{S,N,RD,OD}(similar(R.data, S), copy(R.square_size), copy(R.ragged_lengths), copy(R.ragged_offsets))
# similar with a different ragged size
Base.similar(R::RaggedArray, I::Union{Int,Array{Int},Tuple}...) = similar(R, I)
Base.similar{T}(R::RaggedArray{T}, I::Tuple{Vararg{Union{Int, Array{Int}, Tuple}}}) = similar(R, T, I)
Base.similar{T}(R::RaggedArray, ::Type{T}, I::Union{Int,Array{Int},Tuple}...) = similar(R, T, I)
Base.similar{T}(R::RaggedArray, ::Type{T}, I::Tuple{Vararg{Union{Int, Array{Int}, Tuple}}}) = RaggedArray(T, I...)
# similar with a non-ragged size becomes a regular Array
Base.similar{T}(R::RaggedArray, ::Type{T}, I::Tuple{Vararg{Int}}) = Array(T, I...)

convert_ints() = ()
@inline convert_ints(x, xs...) = (convert(Int, x), convert_ints(xs...)...)

# Note that it is extremely important to *not* listen to the `@inbounds` macro
# or define Base.unsafe_getindex. Base Julia assumes that everything within
# 1:size(A, d) is inbounds, so it may incorrectly flag accesses as `@inbounds`
# that really aren't.
Base.getindex(R::RaggedArray, i::Number...) = getindex(R, convert_ints(i...)...)
function Base.getindex(R::RaggedArray, i::Int...)
    checkbounds(R, i...)
    R.data[ragged_sub2ind(R, i...)] # TODO: turn on @inbounds here (this should be safe)
end
Base.setindex!(R::RaggedArray, v, i::Number...) = setindex!(R, v, convert_ints(i...)...)
function Base.setindex!(R::RaggedArray, v, i::Int...)
    checkbounds(R, i...)
    R.data[ragged_sub2ind(R, i...)] = v
    R
end

"""
    ragged_sub2ind(R::RaggedArray, i::Int...)

Determine the *ragged* linear index from a set of indexing subscripts. Whereas
the normal `sub2ind` would compute the linear indexes in terms of the overall
(maximal) extents of the array, this removes all the invalid indices and assumes
that the given indices are within both the rectangular and ragged bounds.
"""
ragged_sub2ind(R::RaggedArray, i::Int) = i
@generated function ragged_sub2ind{_,__,RD}(R::RaggedArray{_,__,RD}, i::Int...)
    N = length(i)
    inner_idxs = [:(i[$d]) for d=1:RD-1]
    inner_size = Expr(:tuple, [:(size(R, $d)) for d=1:RD-1]...)
    outer_idxs = [:(i[$d]) for d=RD+1:N]
    quote
        inner_size = $inner_size
        inner = sub2ind(inner_size, $(inner_idxs...))
        inner+prod(inner_size)*(i[$RD]-1+R.ragged_offsets[$(outer_idxs...)])
    end
end

import Base: _checkbounds, trailingsize, throw_boundserror
@inline Base.checkbounds(R::RaggedArray, i::AbstractVector{Bool}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::RaggedArray, i::AbstractArray{Bool}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::RaggedArray, i::Union{AbstractArray, Real, Colon}) = checkbounds_impl(R, i)
@inline Base.checkbounds(R::RaggedArray, i::Union{AbstractArray, Real, Colon}...) = checkbounds_impl(R, i...)

# Check both the ragged and rectangular bounds for a RaggedArray
@generated function checkbounds_impl{_,AN,RD}(A::RaggedArray{_,AN,RD}, I...)
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
        push!(args, :(_checkbounds(A.ragged_lengths[$(outer_idxs...)], I[$RD]) || $error))
    else
        # Otherwise, there may be different ragged lengths across the indexes
        push!(args, quote
            raglens = A.ragged_lengths[$(outer_idxs...)] # An array of lengths
            for i in eachindex(raglens)
                _checkbounds(raglens[i], I[$RD]) || $error
            end
        end)
    end
    Expr(:block, meta, args...)
end

# Since we pretend to be a full rectangular array but throw errors for the
# ragged dimensions, we can't just use a linear index through the array...
# but we really are a LinearFast array, so just use a custom type as a flag
immutable LinearIndex <: Integer
    data::Int
end
import Base: <, <=, +, -
<(a::LinearIndex, b::LinearIndex) = a.data < b.data
<=(a::LinearIndex, b::LinearIndex) = a.data <= b.data
+(a::LinearIndex, b::LinearIndex) = LinearIndex(a.data + b.data)
-(a::LinearIndex) = LinearIndex(-a.data)
-(a::LinearIndex, b::LinearIndex) = LinearIndex(a.data - b.data)
Base.convert(::Type{Int}, a::LinearIndex) = a.data
Base.convert(::Type{LinearIndex}, x::Int) = LinearIndex(x)
Base.promote_rule(::Type{LinearIndex}, ::Type{Int}) = LinearIndex

Base.eachindex(A::RaggedArray) = LinearIndex(1):LinearIndex(length(A.data))
Base.getindex(R::RaggedArray, I::LinearIndex) = R.data[I.data]
Base.setindex!(R::RaggedArray, v, I::LinearIndex) = (R.data[I.data] = v)
