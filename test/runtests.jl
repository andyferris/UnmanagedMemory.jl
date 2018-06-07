using Test
using UnmanagedMemory

@testset "UnmanagedArray" begin
    a = UnmanagedArray{Int}(undef, (2,3))
    @test a isa UnmanagedArray{Int, 2}
    @test size(a) == (2, 3)

    a[2,3] = 42
    @test a[2,3] == 42
    @test a[6] == 42
end

@testset "@unmanaged mutable struct" begin
    @unmanaged mutable struct A
        a::Int
    end

    #A(1)
end