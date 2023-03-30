

# Make Figure 3
# Jan, 2023

# Load knee data
data = read.csv('knee_data.csv')

# Meta analysis
meta_fit <- metacont(n.e = n1,
                     mean.e = mu1,
                     sd.e = sigma1,
                     n.c = n2,
                     mean.c = mu2,
                     sd.c = sigma2, 
                     data = data,
                     studlab = labs,
                     fixed = F,
                     random = T,
                     prediction = T,
                     sm = "MD",
                     method.tau = "SJ")
summary(meta_fit)

png(file = 'figure1.png', width = 9, height = 5, res = 600, units = "in") 

forest.meta(meta_fit, 
       sortvar = TE,
       xlim = c(-2.5, 1.5),
       xlab ="Mean difference",
       rightlabs = c("MD","95% CI","Weight"),
       leftlabs = c("Study", "N","Mean","SD","N","Mean","SD"),
       label.e = "Independent",
       label.c = "Transtibial",
       smlab = "",
       print.tau2 = T,
       print.tau2.ci = T,
       print.pval.Q = F,
       col.predict = "black",
       print.I2 = F,
       print.I2.ci = F,
       digits.tau2 = 3,
       digits.sd = 1,
       digits.mean = 1,
       mlab = "",
       ilab.xpos = 8,
       ilab.pos = 8,
       showweights = T)
dev.off()



#### End


