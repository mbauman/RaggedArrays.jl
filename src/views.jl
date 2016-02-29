# Eventually, we may want to support true Ragged views, but this
# hack allows us to get 95% of the functionality without much work.
#
# Simply avoid checking bounds upon construction. TODO: check the rectangular extents
import Base: sub, slice
if VERSION >= v"0.5.0-dev+2663"
    slice(A::AbstractRaggedArray, I::Union{AbstractVector, Real, Colon}...) = SubArray(A, I, map(maximum, index_shape(A, I...)))
    sub(A::AbstractRaggedArray, I::Union{AbstractVector, Real, Colon}...) = SubArray(A, I, map(maximum, index_shape(A, Base.keep_leading_scalars(I)...)))
else
    sub(A::AbstractRaggedArray, I::Union{AbstractVector, Int, Colon}...) = Base.sub_unsafe(A, I)
    slice(A::AbstractRaggedArray, I::Union{AbstractVector, Int, Colon}...) = Base.slice_unsafe(A, I)
    #TODO: I really don't like this... but it's needed for SubArrays
    import Base: *
    *(i::Int, d::RaggedDimension) = i*maximum(d)
end
