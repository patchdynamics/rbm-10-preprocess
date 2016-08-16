
Ev.snake = c('1.00e-009', '1.10e-009', '1.20e-009', '1.30e-009', '1.40e-009', '1.50e-009')
plot(0,0, xlim=c(1,365), ylim=c(0,30), type='n')
for(i in 1:length(Ev.snake)){  
  output = read.csv(paste0('../ModelOutputs/Sensitivity.', Ev.snake[i], '.Snake.txt'), sep=' ', header = FALSE);
  names(output) = names.output.snake
  lines(output$Snake10.T)
}