
snake.hydraulics = read.csv('../Data/Hydraulics/SnakeHydraulics.csv')

# Segment Description
#FORMAT(7X,a20,5x,a5,10x,3f5.0)
filename = 'Snake.Hydraulics.txt'
write('', file=filename) # clear the file
for(i in 1:nrow(snake.hydraulics)){
  hydraulics = snake.hydraulics[i,]
  write( sprintf('R21.L1 %10s     %5s          %5.1f%5.1f%5.1f',     # .1 is diff from FORTRAN file (?)
                'Snake River', 'RIVER', hydraulics$RM.Begin, hydraulics$RM.End, hydraulics$Elevation),
         file=filename, append=TRUE )
  # number of computational elements per segmentation, weather file to be used, 
  # headwaters number, number of entering tributaries, 
  # reach number if the tributary is one for which temperatures are simulated
  write( sprintf('R21.L2 %5i%5i%5i%5i%5i', 5, 3, 0, 
                 if(hydraulics$Tribs != '') 1 else 0, 0),
         file=filename, append=TRUE )
  # a area, b area, a width, b width
  write( sprintf('R21.L3 %10.3f%10.3f%10.3f%10.3f',   # FORTRAN file lists %10.0f (?)
                 hydraulics$A.Area, hydraulics$B.Area,
                 hydraulics$A.Width, hydraulics$B.Width),
         file=filename, append=TRUE )
  write( 'R21.L4 End of Segment #4', file=filename, append=TRUE)
  write( '', file=filename, append=TRUE)
}