module RaggedArrays

include("abstract.jl")
include("indexing.jl")
include("array.jl")
include("show.jl")

export RaggedArray, AbstractRaggedArray, raggedlengths, rectsize

end # module
