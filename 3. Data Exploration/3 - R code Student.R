marketing <- read.csv("C:\\Users\\PC 11\\Desktop\\P152419\\BUSINESS ANALYTICS\\Ch3_marketing.csv")

install.packages("corrgram")
library(corrgram)

corrgram(marketing[ ,1:6], order = TRUE,
         main = 'try',
         lower.panel = panel.bar,
         upper.panel = panel.pie,
         diag.panel = panel.minmax)
?lm()
