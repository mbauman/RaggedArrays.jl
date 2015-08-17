using RaggedArrays
using Base.Test

# write your own tests here
R = RaggedArray(Int, (3,2,4,1), 4)
fill!(R, 0)
@test all(R.data .== 0)
for elt in R
    @test elt == 0
end
@test R[1,1] == 0
@test R[2,1] == 0
@test R[3,1] == 0
@test_throws BoundsError R[4,1] == 0
@test R[1,2] == 0
@test R[2,2] == 0
@test_throws BoundsError R[3,2] == 0
@test_throws BoundsError R[4,2] == 0
@test R[1,3] == 0
@test R[2,3] == 0
@test R[3,3] == 0
@test R[4,3] == 0
@test R[1,4] == 0
@test_throws BoundsError R[2,4] == 0
@test_throws BoundsError R[3,4] == 0
@test_throws BoundsError R[4,4] == 0

@test all(R[:, 1] .== 0)
@test all(R[:, 2] .== 0)
@test all(R[:, 3] .== 0)
@test all(R[:, 4] .== 0)

@test all(R[1, :] .== 0)
@test_throws BoundsError R[2, :]
@test_throws BoundsError R[3, :]
@test_throws BoundsError R[4, :]

@test R[:, :] == R
@test all(collect(R) .== 0)

[R[i] = Int(i) for i in eachindex(R)]

@test R[1,1] == 1
@test R[2,1] == 2
@test R[3,1] == 3
@test_throws BoundsError R[4,1] == 0
@test R[1,2] == 4
@test R[2,2] == 5
@test_throws BoundsError R[3,2] == 0
@test_throws BoundsError R[4,2] == 0
@test R[1,3] == 6
@test R[2,3] == 7
@test R[3,3] == 8
@test R[4,3] == 9
@test R[1,4] == 10
@test_throws BoundsError R[2,4] == 0
@test_throws BoundsError R[3,4] == 0
@test_throws BoundsError R[4,4] == 0

@test R[:, 1] == [1,2,3]
@test R[:, 2] == [4,5]
@test R[:, 3] == [6,7,8,9]
@test R[:, 4] == [10,]

@test R[1, :] == [1 4 6 10]
@test_throws BoundsError R[2, :]
@test_throws BoundsError R[3, :]
@test_throws BoundsError R[4, :]

@test R[:, :] == R

##

R = RaggedArray(Int, 4, [3,1,2], 3)
[R[i] = Int(i) for i in eachindex(R)]

@test R[:,:,1] == R[:,1:3,1] == reshape(1:4*3, 4, 3)
@test R[:,:,2] == R[:,1:1,2] == reshape(4*3+1:4*3+4*1, 4, 1)
@test R[:,:,3] == R[:,1:2,3] == reshape(4*4+1:4*4+4*2, 4, 2)
@test R[:,:,:] == R

@test R[trues(4),:,1] == R[trues(4),1:3,1] == reshape(1:4*3, 4, 3)
@test R[trues(4),:,2] == R[trues(4),1:1,2] == reshape(4*3+1:4*3+4*1, 4, 1)
@test R[trues(4),:,3] == R[trues(4),1:2,3] == reshape(4*4+1:4*4+4*2, 4, 2)

@test R[4:-1:1,:,1] == R[4:-1:1,1:3,1] == flipdim(reshape(1:4*3, 4, 3), 1)
@test R[4:-1:1,:,2] == R[4:-1:1,1:1,2] == flipdim(reshape(4*3+1:4*3+4*1, 4, 1), 1)
@test R[4:-1:1,:,3] == R[4:-1:1,1:2,3] == flipdim(reshape(4*4+1:4*4+4*2, 4, 2), 1)

@test R == R[:,:,:] == R[trues(4), :, trues(3)] == R[1:4, :, 1:3]

[R[i] = Int(i)+10^5 for i in eachindex(R)]
s = sprint(show, R)
for i = 1:length(R)
    @test contains(s, string(i+10^5))
end
s = sprint(writemime, MIME"text/plain"(), R)
for i = 1:length(R)
    @test contains(s, string(i+10^5))
end
sprint(writemime, MIME"text/plain"(), RaggedArray(Float64, [1:20;20:-1:1], 20, 2))
##

R = RaggedArray(Int, [0,0,0,0,0,3],2,3)
[R[i] = Int(i) for i in eachindex(R)]

@test R[:,:,:] == R
for k=1:2, j=1:3, i=1:3
    if i==3 && i==2
        @test R[i,j,k] == k
    else
        @test_throws BoundsError R[j,j]
    end
end
@test R[:,2,3] == [1,2,3]

R[1,2,3] = 3
R[2,2,3] = 2
R[3,2,3] = 1
@test R[:,2,3] == collect(R) == [3,2,1]

@test_throws BoundsError R[1,1,1] = 1


#

R = RaggedArray(Int, [0,0,0],3)
@test isempty(R)

# 

@test_throws ArgumentError RaggedArray(Int, [1,2,3,4,5,6], [4,5,6], 2)
@test_throws ArgumentError RaggedArray(Int, [1,2,3,4,5,6])

