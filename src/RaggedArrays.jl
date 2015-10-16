module RaggedArrays

include("abstract.jl")
include("indexing.jl")
include("array.jl")
include("show.jl")
include("raggedrangematrix.jl")

export RaggedArray, AbstractRaggedArray, RaggedRangeMatrix, RaggedDimension, raggedlengths, rectsize

end # module
