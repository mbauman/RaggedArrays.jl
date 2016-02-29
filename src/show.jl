function Base.summary{T,N,RD}(a::AbstractRaggedArray{T,N,RD})
    dims = join(map(d->(d == RD ? "??" : string(size(a, d))), 1:ndims(a)), 'x')
    string(dims, " ", typeof(a))
end
