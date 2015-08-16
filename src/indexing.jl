# Nonscalar indexing is so close to being able to punt to the base library...
# were it not for colons along the ragged dimension.
# This is adapted from JuliaLang/julia#12567
to_index(I::AbstractArray{Bool}) = find(I)
to_index(I::AbstractArray) = I
to_index(i) = i
# shape of array to create for getindex() with indexes I, dropping trailing scalars
index_shape(A::RaggedArray, I::AbstractArray) = error("linear indexing not supported")
index_shape(A::RaggedArray, I::AbstractArray{Bool}) = error("linear indexing not supported")
index_shape(A::RaggedArray, I::Colon) = error("linear indexing not supported")
@generated function index_shape{_,__,RD}(A::RaggedArray{_,__,RD}, I...)
    N = length(I)
    sz = Expr(:tuple)
    outer_idxs = [:(I[$d]) for d=RD+1:N]
    for d=1:N
        if !any(i->i<:Union{AbstractArray,Colon}, I[d:end])
            break
        elseif I[d] <: Colon
            if d == RD
                # If there's a Colon over the ragged dimension, return the
                # selected ragged size(s). If the outer indices aren't all
                # scalars, this will return an array and force the creation
                # of a new RaggedArray for the indexed output.
                push!(sz.args, :(A.ragged_lengths[$(outer_idxs...)]))
            else
                push!(sz.args, d < N ? :(size(A, $d)) : :(trailingsize(A, Val{$d})))
            end
        elseif I[d] <: AbstractArray{Bool}
            push!(sz.args, :(sum(I[$d])))
        elseif I[d] <: AbstractVector
            push!(sz.args, :(length(I[$d])))
        else
            push!(sz.args, 1)
        end
    end
    quote
        $(Expr(:meta, :inline))
        $sz
    end
end

using Base.Cartesian
import Base: cartindex_exprs, checksize, unsafe_getindex
indexref(idx, i::Int) = idx
indexref(::Colon, i::Int) = i
@inline indexref(A::AbstractArray, i::Int) = unsafe_getindex(A, i)
# Based on linear slow getindex for now
@generated function Base.getindex{T,AN,RD}(A::RaggedArray{T,AN,RD}, I...)
    meta = Expr(:meta, :inline)
    N = length(I)
    Isplat = [:(I[$d]) for d=1:N]
    # Expand any cartesian indices first
    any(i->i<:CartesianIndex, I) && return :($meta; getindex(A, $(cartindex_exprs(I, :I)...)))
    !any(i->i<:Union{AbstractArray,Colon}, I) && return :(throw(ArgumentError("unsupported index types $I")))
    quote
        checkbounds(A, $(Isplat...))
        @nexprs $N d->(I_d = to_index(I[d]))
        dest = similar(A, @ncall $N index_shape A I)
        @ncall $N checksize dest I # TODO: stricter checksize for ragged arrays?
        @ncall $N getindex! dest A I
    end
end

@generated function getindex!(dest::AbstractArray, src::RaggedArray, I...)
    N = length(I)
    quote
        D = eachindex(dest)
        Ds = start(D)
        @nloops $N i dest d->(j_d = indexref(I[d], i_d)) begin
            d, Ds = next(D, Ds)
            v = @ncall $N getindex src j
            setindex!(dest, v, d)
        end
        dest
    end
end

# Adapted from Base.Cartesian's @nloops: ragged iteration!
macro ragged_nloops(N, itersym, rangeexpr, args...)
    _ragged_nloops(N, itersym, rangeexpr, args...)
end
import Base.Cartesian: inlineanonymous
function _ragged_nloops(N::Int, RD::Int, itersym::Symbol, arraysym::Symbol, args::Expr...)
    if !(1 <= length(args) <= 3)
        throw(ArgumentError("number of arguments must be 1 ≤ length(args) ≤ 3, got $nargs"))
    end
    body = args[end]
    ex = Expr(:escape, body)
    for dim = 1:N
        itervar = inlineanonymous(itersym, dim)
        rng = dim == RD ? :(1:size($arraysym, $dim, $(Expr(:tuple, [symbol(itersym, :_, d) for d=RD+1:N]...)))) :
                          :(1:size($arraysym, $dim))
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

@generated function getindex!{_,__,RD}(dest::RaggedArray{_,__,RD}, src::RaggedArray, I...)
    N = length(I)
    quote
        D = eachindex(dest)
        Ds = start(D)
        @ragged_nloops $N $RD i dest d->(j_d = indexref(I[d], i_d)) begin
            d, Ds = next(D, Ds)
            v = @ncall $N getindex src j
            setindex!(dest, v, d)
        end
        dest
    end
end

