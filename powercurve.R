library(tidyverse)
setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Vestas V150-4.0-4.2 MW PO1.txt'
df <- read.table(path, col.names=c('speed', 'power')) %>% add_row(speed=26.5, power=0)
df %>% ggplot(aes(x=speed, y=power)) + geom_line()
