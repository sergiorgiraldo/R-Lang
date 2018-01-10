library("matlib")
#2x+y=6
#3x-4y=-13
A <- matrix(c(2,3,1,-4),2,2)
B <- c(6,-13)
showEqn(A,B)
plotEqn(A,B)
Solve(A,B)

#2x+y+z=7
#3x-4y +5z=-8
#x+y+z=6
A <- matrix(c(2,3,1,1,-4,1,1,5,1),3,3)
B <- c(7,-8,6)
showEqn(A,B)
plotEqn3d(A,B)
Solve(A,B)
