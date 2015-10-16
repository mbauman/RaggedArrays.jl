# RaggedArrays

[![Build Status](https://travis-ci.org/mbauman/RaggedArrays.jl.svg?branch=master)](https://travis-ci.org/mbauman/RaggedArrays.jl) [![Coverage Status](https://coveralls.io/repos/mbauman/RaggedArrays.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/mbauman/RaggedArrays.jl?branch=master)

RaggedArrays.jl is an experimental package that tries to make a valid contiguous ragged (or jagged) array behave as normally as possible.  The array "pretends" to have a size that spans the maximum extent of the ragged lengths, but accessing beyond a ragged length is a BoundsError.  Specify the ragged dimension's sizes as an array argument to the constructor:

```jl
julia> R = RaggedArray(Int, [3,1,2,4], 4) # Varying column lengths
??x4 RaggedArrays.RaggedArray{Int64,2,1,1}:
 4593457072     4590781776     4590781776      4590781776
 4592320176            #undef  4590780624      4590780624
 4592320176            #undef         #undef   4590781776
        #undef         #undef         #undef  13197973264

julia> [R[i]=i for i=eachindex(R)]; R
??x4 RaggedArrays.RaggedArray{Int64,2,1,1}:
   1       4       5      7
   2     #undef    6      8
   3     #undef  #undef   9
 #undef  #undef  #undef  10

julia> R = RaggedArray(Int, 3, [3,1,2,4], 4); # Varying row lengths
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
```

There is also a specialized RaggedRangeMatrix type that allows collections of
ranges to behave like a ragged matrix. It depends upon
[RangeArrays.jl](https://github.com/mbauman/RangeArrays.jl) in order to return a
rectangular RangeMatrix when possible:

```jl
julia> RR = RaggedRangeMatrix(1:3,3:6,7:12,-2:1)
??x4 RaggedArrays.RaggedRangeMatrix{Int64,Array{UnitRange{Int64},1}}:
   1       3      7   -2
   2       4      8   -1
   3       5      9    0
 #undef    6     10    1
 #undef  #undef  11  #undef
 #undef  #undef  12  #undef

julia> RR[1:3,2:4]
3x3 RangeArrays.RangeMatrix{Int64,Array{UnitRange{Int64},1}}:
 3  7  -2
 4  8  -1
 5  9   0

julia> RR[:, 3]
7:12
```
