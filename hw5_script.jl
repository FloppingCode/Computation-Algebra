# Define problem statement
include("./hw5_essentials.jl")

t = Monomial(4,1//1,[1,0,0,0])
x = Monomial(4,1//1,[0,1,0,0])
y = Monomial(4,1//1,[0,0,1,0])
z = Monomial(4,1//1,[0,0,0,1])

p1 = t*t+x*x+y*y+z*z
@show p1