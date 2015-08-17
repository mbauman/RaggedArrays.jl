# RaggedArrays

[![Build Status](https://travis-ci.org/mbauman/RaggedArrays.jl.svg?branch=master)](https://travis-ci.org/mbauman/RaggedArrays.jl)

RaggedArrays.jl is an experimental package that tries to make a valid contiguous ragged (or jagged) array behave as normally as possible.  The array "pretends" to have a size that spans the maximum extent of the ragged lengths, but accessing beyond a ragged length is a BoundsError.  Specify the ragged dimension's sizes as an array argument to the constructor:

```jl
julia> R = RaggedArrays.RaggedArray(Int, [3,1,2,4], 4) # Varying column lengths
??x4 RaggedArrays.RaggedArray{Int64,2,1,1}:
 4593457072     4590781776     4590781776      4590781776
 4592320176            #undef  4590780624      4590780624
 4592320176            #undef         #undef   4590781776
        #undef         #undef         #undef  13197973264

julia> [R[i]=i for i=eachindex(R)];

julia> R
??x4 RaggedArrays.RaggedArray{Int64,2,1,1}:
   1       4       5      7
   2     #undef    6      8
   3     #undef  #undef   9
 #undef  #undef  #undef  10
 
 julia> R = RaggedArrays.RaggedArray(Int, 3, [3,1,2,4], 4); # Varying row lengths
       [R[i]=i for i=eachindex(R)];
       R
3x??x4 RaggedArrays.RaggedArray{Int64,3,2,1}:
[:, :, 1] =
 1  4  7  #undef
 2  5  8  #undef
 3  6  9  #undef

[:, :, 2] =
 10  #undef  #undef  #undef
 11  #undef  #undef  #undef
 12  #undef  #undef  #undef

[:, :, 3] =
 13  16  #undef  #undef
 14  17  #undef  #undef
 15  18  #undef  #undef

[:, :, 4] =
 19  22  25  28
 20  23  26  29
 21  24  27  30
 
 julia> R[1:3, 1:3, 1] # Indexing a non-ragged subsection returns an Array
3x3 Array{Int64,2}:
 1  4  7
 2  5  8
 3  6  9
 
 julia> R[2, :, 2:4] # Indexing with a : in the ragged dimension returns a RaggedArray
1x??x3 RaggedArrays.RaggedArray{Int64,3,2,1}:
[:, :, 1] =
 11  #undef  #undef  #undef

[:, :, 2] =
 14  17  #undef  #undef

[:, :, 3] =
 20  23  26  29
 
 julia> R[2:3, :, 3] # But only if it spans more than one of the outer dimensions
2x2 Array{Int64,2}:
 14  17
 15  18
