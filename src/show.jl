function Base.summary{T,N,RD}(a::RaggedArray{T,N,RD})
    dims = join(map(d->(d == RD ? "??" : string(size(a, d))), 1:ndims(a)), 'x')
    string(dims, " ", typeof(a))
end

import Base: print_matrix, sub_unsafe
# This is a crazy whack-a-mole hack, but it works... at least for now...
function Base.show_nd(io::IO, a::AbstractRaggedArray, limit, print_matrix, label_slices)
    if isempty(a)
        return
    end
    tail = size(a)[3:end]
    nd = ndims(a)-2
    for I in CartesianRange(tail)
        idxs = I.I
        if limit
            for i = 1:nd
                ii = idxs[i]
                if size(a,i+2) > 10
                    if ii == 4 && all(x->x==1,idxs[1:i-1])
                        for j=i+1:nd
                            szj = size(a,j+2)
                            if szj>10 && 3 < idxs[j] <= szj-3
                                @goto skip
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        @goto skip
                    end
                    if 3 < ii <= size(a,i+2)-3
                        @goto skip
                    end
                end
            end
        end
        if label_slices
            print(io, "[:, :, ")
            for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
            println(io, idxs[end], "] =")
        end
        slice = sub_unsafe(a, (1:size(a,1), 1:size(a,2), idxs...))
        print_matrix(io, slice)
        print(io, idxs == tail ? "" : "\n\n")
        @label skip
    end
end
