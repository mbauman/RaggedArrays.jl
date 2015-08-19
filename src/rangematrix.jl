"""
    RaggedRangeMatrix{T<:Range}(rs::AbstractVector{T})

A RaggedRangeMatrix is a simple matrix representation of a vector of ranges,
with each range representing one column. Make a vector of ranges behave like a
ragged matrix by passing it to the constructor. The ranges may have different
lengths and the vector of ranges is not copied.
"""
immutable RaggedRangeMatrix{T,A} <: AbstractRaggedArray{T,2,1,1}
    rs::A # A <: AbstractVector{_<:Range{T}}
    # TODO: maybe cache the maximum extents (maxlengths(rs), length(rs))?
end
RaggedRangeMatrix{T<:Range}(rs::AbstractVector{T}) = RaggedRangeMatrix{eltype(T), typeof(rs)}(rs)

# Simple devectorization helper functions
function maxlengths(xs::AbstractVector)
    m = 0
    for x in xs
        m = max(length(x), m)
    end
    m
end
function sumlengths(xs::AbstractVector)
    s = 0
    for x in xs
        s += length(x)
    end
    s
end
maplength{T<:Range}(xs::AbstractVector{T}) = [length(x) for x in xs]
maplength(x::Range) = length(x)

Base.length(R::RaggedRangeMatrix) = sumlengths(R.rs)
Base.size(R::RaggedRangeMatrix) = (maxlengths(R.rs), length(R.rs))
function Base.size(R::RaggedRangeMatrix, d)
    d == 1 && return maxlengths(R.rs)
    d == 2 && return length(R.rs)
    d > 2 && return 1
    throw(ArgumentError())
end
@inline raggedlengths(R::RaggedRangeMatrix, idxs...) = maplength(R.rs[idxs...])

# Scalar indexing
Base.getindex(R::RaggedRangeMatrix, i::Int, j::Int) = (checkbounds(R, i, j); ragged_unsafe_getindex(R, i, j))
ragged_unsafe_getindex(R::RaggedRangeMatrix, i::Int, j::Int) = @inbounds return R.rs[j][i]

# For non-scalar indexing, only specialize with inner Ranges and Colons to
# return Ranges or RangeMatrixes. For everything else, we can use the fallbacks.
Base.getindex(R::RaggedRangeMatrix, I::Union{Range, Colon}, J) = (checkbounds(R, I, J); ragged_unsafe_getindex(R, I, J))
# This is intentionally not spelled Base.unsafe_getindex!
ragged_unsafe_getindex(R::RaggedRangeMatrix, I::Range, j::Number) = @inbounds return R.rs[j][I]
ragged_unsafe_getindex(R::RaggedRangeMatrix, c::Colon, j::Number) = @inbounds return R.rs[j][c]
ragged_unsafe_getindex(R::RaggedRangeMatrix, I::Range, ::Colon)   = @inbounds return RaggedRangeMatrix([R.rs[j][I] for j=1:length(R.rs)]) # TODO: use RangeMatrix!
ragged_unsafe_getindex(R::RaggedRangeMatrix, I::Range, J)         = @inbounds return RaggedRangeMatrix([R.rs[j][I] for j in J])           # TODO: use RangeMatrix!
ragged_unsafe_getindex(R::RaggedRangeMatrix, c::Colon, ::Colon)   = @inbounds return RaggedRangeMatrix([R.rs[j][c] for j=1:length(R.rs)])
ragged_unsafe_getindex(R::RaggedRangeMatrix, c::Colon, J)         = @inbounds return RaggedRangeMatrix([R.rs[j][c] for j in J])
