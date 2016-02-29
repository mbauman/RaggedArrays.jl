# Nonscalar indexing is so close to being able to punt to the base library...
# were it not for colons along the ragged dimension.
# This is adapted from JuliaLang/julia#12567
to_index(I::AbstractArray{Bool}) = find(I)
to_index(I::AbstractArray) = I
to_index(i) = i
import Base: index_shape, index_shape_dim, index_lengths, index_lengths_dim
# shape of array to create for getindex() with indexes I, dropping trailing scalars
index_shape_dim{T,N,RD}(A::AbstractRaggedArray{T,N,RD}, dim, ::Colon) =
    dim == RD ? throw(ArgumentError("linear indexing through a ragged dimension is unsupported")) : trailingsize(A, dim)
index_shape_dim{T,N,RD}(A::AbstractRaggedArray{T,N,RD}, dim, ::Colon, i, I...) =
    (dim == RD ? RaggedDimension(raggedlengths(A, i, I...)) : size(A, dim), index_shape_dim(A, dim+1, i, I...)...)

index_lengths_dim{T,N,RD}(A::AbstractRaggedArray{T,N,RD}, dim, ::Colon) =
    dim == RD ? throw(ArgumentError("linear indexing through a ragged dimension is unsupported")) : trailingsize(A, dim)
index_lengths_dim{T,N,RD}(A::AbstractRaggedArray{T,N,RD}, dim, ::Colon, i, I...) =
    (dim == RD ? RaggedDimension(raggedlengths(A, i, I...)) : size(A, dim), index_lengths_dim(A, dim+1, i, I...)...)

abstract RaggedIndexing <: Base.LinearIndexing
immutable RaggedFast <: RaggedIndexing; end
immutable RaggedSlow <: RaggedIndexing; end

# For fast linear indexing, we create a custom integer type that can act as a
# flag for callers who opt-in to ragged behaviors. Linear indexing with regular
# Ints is ambiguous.
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

Base.eachindex(::RaggedFast, R::AbstractArray) = LinearIndex(1):LinearIndex(length(R))
function Base.eachindex(::RaggedFast, R::AbstractArray, Rs::AbstractArray...)
    # ensure all sizes are equal; see JuliaLan/julia#13310
    sz = size(R)
    for r in Rs
        sz == size(r) || throw(DimensionMismatch())
    end
    LinearIndex(1):LinearIndex(length(R))
end

using Base.Cartesian
import Base: cartindex_exprs, checksize, unsafe_getindex
indexref(idx, i::Int) = idx
indexref(::Colon, i::Int) = i
@inline indexref(A::AbstractArray, i::Int) = unsafe_getindex(A, i)
convert_ints() = ()
@inline convert_ints(x, xs...) = (convert(Int, x), convert_ints(xs...)...)
@noinline throw_checksize_error(A, shp) = throw(ArgumentError("output array is the wrong size; expected $shp, got $(size(A))"))

# Just use one big generated method to do all the fallback dispatch in one place.
@generated function Base.getindex{T,AN,RD}(A::AbstractRaggedArray{T,AN,RD}, I...)
    meta = Expr(:meta, :inline)
    N = length(I)
    Isplat = [:(I[$d]) for d=1:N]
    # Expand any cartesian indices first
    any(i->i<:CartesianIndex, I) && return :($meta; getindex(A, $(cartindex_exprs(I, :I)...)))
    # If we're explicitly using linear indexing, use ragged_ind2sub
    length(I) == 1 && I[1] <: LinearIndex && return :($meta; getindex(A, ragged_ind2sub(A, I[1])...))
    # Then ensure we're not linear indexing
    N <= RD && return :(throw(ArgumentError("linear indexing through a ragged dimension is unsupported")))
    # If all indices are Int, then the AbstractRaggedArray subtype didn't define indexing
    all(i->i<:Int, I) && return :(error("indexing not defined for $(typeof(A))"))
    # Convert scalar indexing with numbers to integers so subtypes just define ::Int
    all(i->i<:Number, I) && return :($meta; getindex(A, convert_ints(I...)...))
    !any(i->i<:Union{AbstractArray,Colon}, I) && return :(throw(ArgumentError("unsupported indexes $I")))
    # Finally, do multi-dimensional indexing, based on linear slow getindex
    quote
        checkbounds(A, $(Isplat...))
        @nexprs $N d->(I_d = to_index(I[d]))
        shp = @ncall $N index_shape A I
        dest = similar(A, shp)
        size(dest) == shp || throw_checksize_error(A, shp)
        @ncall $N getindex! dest A I
    end
end

# We define our own unsafe operators, since Base methods assume that the
# unsafe_* methods are always "safe" within 1:size(A, d).
@inline ragged_unsafe_getindex(R::AbstractRaggedArray, I...) = getindex(R, I...)
@inline ragged_unsafe_setindex!(R::AbstractRaggedArray, v, I...) = setindex!(R, v, I...)

# Assigning output to a non-ragged array
@generated function getindex!(dest::AbstractArray, src::AbstractRaggedArray, I...)
    N = length(I)
    quote
        D = eachindex(dest)
        Ds = start(D)
        idxlens = index_lengths(src, I...)
        @nloops $N i d->(1:idxlens[d]) d->(j_d = indexref(I[d], i_d)) begin
            d, Ds = next(D, Ds)
            v = @ncall $N ragged_unsafe_getindex src j
            Base.unsafe_setindex!(dest, v, d)
        end
        dest
    end
end

# Adapted from Base.Cartesian's @nloops: ragged iteration!
macro ragged_nloops(N, RD, itersym, rangeexpr, args...)
    _ragged_nloops(N, RD, itersym, rangeexpr, args...)
end
import Base.Cartesian: inlineanonymous
function _ragged_nloops(N::Int, RD::Int, itersym::Symbol, rangeexpr, args::Expr...)
    if !(1 <= length(args) <= 3)
        throw(ArgumentError("number of arguments must be 1 ≤ length(args) ≤ 3, got $nargs"))
    end
    body = args[end]
    ex = Expr(:escape, body)
    for dim = 1:N
        itervar = inlineanonymous(itersym, dim)
        if isa(rangeexpr, Symbol)
            rng = dim == RD ? :(1:raggedlengths($arraysym, $([symbol(itersym, :_, d) for d=RD+1:N]...))) :
                              :(1:size($arraysym, $dim))
        else
            rng = dim == RD ? :(1:$(inlineanonymous(rangeexpr, dim))[$([symbol(itersym, :_, d) for d=RD+1:N]...)]) :
                              :(1:$(inlineanonymous(rangeexpr, dim)))
        end
        preexpr = length(args) > 1 ? inlineanonymous(args[1], dim) : (:(nothing))
        postexpr = length(args) > 2 ? inlineanonymous(args[2], dim) : (:(nothing))
        ex = quote
            for $(esc(itervar)) = $(esc(rng))
                $(esc(preexpr))
                $ex
                $(esc(postexpr))
            end
        end
    end
    ex
end

# Assigning output to a ragged array
@generated function getindex!{_,__,RD}(dest::AbstractRaggedArray, src::AbstractRaggedArray{_,__,RD}, I...)
    N = length(I)
    quote
        D = eachindex(dest)
        Ds = start(D)
        idxlens = index_lengths(src, I...)
        @ragged_nloops $N $RD i d->(idxlens[d]) d->(j_d = indexref(I[d], i_d)) begin
            d, Ds = next(D, Ds)
            v = @ncall $N ragged_unsafe_getindex src j
            setindex!(dest, v, d) # TODO: this can become unsafe once checksize is stricter
        end
        dest
    end
end

## Eachindex
# Doc: "If the arrays have different sizes and/or dimensionalities, eachindex
# returns an iterable that spans the largest range along each dimension."

Base.linearindexing(::RaggedFast, ::RaggedFast) = RaggedFast()
Base.linearindexing(::RaggedIndexing, ::RaggedIndexing) = RaggedSlow()
Base.linearindexing(::RaggedIndexing, ::Base.LinearIndexing) = Base.LinearSlow()
Base.linearindexing(::Base.LinearIndexing, ::RaggedIndexing) = Base.LinearSlow()

Base.linearindexing{R<:AbstractRaggedArray}(::Type{R}) = RaggedSlow()

Base.eachindex{T,N,RD}(::RaggedSlow, R::AbstractRaggedArray{T,N,RD}) = RaggedCartesianRange(size(R))
# TODO: extend to maximum of each dimension?  For now, just ensure sizes are equal and punt.
function Base.eachindex(::RaggedSlow, R::AbstractArray, Rs::AbstractArray...)
    sz = size(R)
    for r in Rs
        sz == size(r) || throw(DimensionMismatch())
    end
    eachindex(RaggedSlow(), R)
end
Base.eachindex(::RaggedSlow, A::AbstractArray) = eachindex(Base.LinearSlow(), A)


## Cartesian iteration and eachindex
immutable RaggedCartesianRange{N,RD,R}
    start::NTuple{N,Int}
    stop::NTuple{N,Int}
    rags::R # Break out the ragged lengths to ensure type-stability in stop[i]
end
@generated function RaggedCartesianRange{N}(stop::NTuple{N})
    start = Expr(:tuple)
    start.args = ones(Int,N)
    :(RaggedCartesianRange($start, stop))
end
@generated function RaggedCartesianRange{N}(start::NTuple{N,Int}, stop::NTuple{N})
    RD = 0
    for i = 1:N
        stop.parameters[i] <: Number && continue
        if RD == 0
            RD = i
        else
            return :(throw(ArgumentError("more than one ragged dimension given in stop")))
        end
    end
    RD == 0 && return :(throw(ArgumentError("no ragged dimension given in stop; use a normal CartesianRange instead")))
    N == RD && return :(throw(ArgumentError("ragged dimension must not be the last dimension given")))
    intstop = Expr(:tuple)
    intstop.args = Any[d==RD ? :(typemax(Int)) : :(stop[$d]) for d=1:N]
    quote
        rags = stop[$RD]
        RaggedCartesianRange{$N,$RD,typeof(rags)}(start, $intstop, rags)
    end
end

@generated function Base.start{N,RD}(iter::RaggedCartesianRange{N,RD})
    outer_starts = Any[:(iter.start[$d]) for d=RD+1:N]
    outer_stops = Expr(:tuple)
    outer_stops.args = Any[:(iter.stop[$d]) for d=RD+1:N]

    extest = Expr(:||)
    extest.args = Any[:(start[$d] > iter.stop[$d]) for d = 1:N]
    extest.args[RD] = :(slice > length(iter.rags) || start[$RD] > iter.rags[slice])
    # If any start > stop, jump to stop + 1 in the last dimension
    exstop = Expr(:tuple)
    exstop.args = Any[d < N ? :(iter.start[$d]) : :(iter.stop[$N]+1) for d = 1:N]
    quote
        slice = sub2ind($outer_stops, $(outer_starts...))
        start = iter.start
        # increment slice and state to first nonzero ragged dimension
        while slice < length(iter.rags) && iter.rags[slice] == 0
            (start, (slice, start)) = next(iter, (slice, start))
        end
        (slice, $extest ? $exstop : start)
    end
end

import Base.Cartesian: inlineanonymous, @nexprs
macro ragged_nif(N, RD, condition, operation, ragged_condition, ragged_operation)
    # Handle the final "else"
    ex = esc(inlineanonymous(operation, N))
    # Make the nested if statements
    for i = N-1:-1:1
        if i == RD
            ex = Expr(:if, esc(inlineanonymous(ragged_condition,i)), esc(inlineanonymous(ragged_operation,i)), ex)
        else
            ex = Expr(:if, esc(inlineanonymous(condition,i)), esc(inlineanonymous(operation,i)), ex)
        end
    end
    ex
end

@generated function Base.next{N,RD}(iter::RaggedCartesianRange{N,RD}, state)
    meta = Expr(:meta, :inline)
    quote
        $meta
        slice, index = state
        @ragged_nif($N, $RD,
            d->(index[d] < iter.stop[d]),
                d->(@nexprs($N, k->(ind_k = ifelse(k>=d, index[k] + (k==d), iter.start[k]))); slice = slice + (d>$RD)),
            d->(index[d] < iter.rags[slice]), # TODO: what to do about the bounds check here?
                d->(@nexprs($N, k->(ind_k = ifelse(k>=d, index[k] + (k==d), iter.start[k])))))
        newindex = @ncall $N CartesianIndex{$N} ind
        if slice <= length(iter.rags) && iter.rags[slice] == 0
            # Skip past zero-length ragged arrays by moving the newindex ahead
            CartesianIndex{$N}(index), next(iter, (slice, newindex))[2]
        else
            CartesianIndex{$N}(index), (slice, newindex)
        end
    end
end
function Base.done{N}(iter::RaggedCartesianRange{N}, state)
    slice, index = state
    index[N] > iter.stop[N]
end

@generated function Base.length{N,RD}(iter::RaggedCartesianRange{N,RD})
    inner_sz = [:(iter.stop[$i]-iter.start[$i]+1) for i=1:RD-1]
    :(sum(iter.rags) * prod(($(inner_sz...),)))
end

# Add this as an option for the CartesianRange Tuple constructor
Base.CartesianRange(stop::Tuple{Vararg{Union{Int,RaggedDimension}}}) = RaggedCartesianRange(stop)
