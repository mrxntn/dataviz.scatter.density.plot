# ========== # ========== # ========== # ========== # ========== #

# 

# ========== # ========== # ========== # ========== # ========== #
# library
library(ggplot2) # data viz
library(gridExtra) # arrange multiple plots
library(dplyr)

# ========== # ==========
# set wd
getwd()
setwd(dir = "/Users/amx/data_local/R/github/scatter.density.plot")

# ========== # ==========
# read csv
data <- read.csv(file = "scatter.density.data.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
data.df <- data.frame(data)
View(data.df)

# ========== # ==========
# plot and print png file
setwd(dir = "/Users/amx/data_local/R/github/scatter.density.plot")

png(filename="scatter.density.png", width=1024, height=768, units="px")
par(mfrow=c(1,1))

# scatter plot
scat.1 <- ggplot(data.df, aes(cvms_cvfs_1, SEC_1)) + # ggplot(data, aes(x, y))
  
  # data points (slightly jittered to avoid overplotting)
  geom_point(shape=19, size = 5, alpha=0.7, position=position_jitter(width=.15, height=.1), color="dodgerblue2") +
  
  # regression line
  geom_smooth(method=lm, se=FALSE, color = "red") +
  
  # axis titles
  labs(title="", x = "Variable X", y = "Variable Y") +
  
  # theme (no axis ticks, change text size, grid lines in grey)
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        panel.grid.major.y = element_line(colour = "grey90"))
scat.1

# density plot right
dens.1 <- ggplot(data.df, aes(SEC_1)) +
  
  # density
  geom_density(alpha=0.4, color = "dodgerblue2", fill = "dodgerblue2") +
  
  # data rug
  geom_rug(aes(x = SEC_1, y = 0), color = "dodgerblue2", position = position_jitter(width = 0.05,height = 0), alpha = 0.5) + 
  
  # axis titles
  labs(title="", x="", y = "Density") +
  
  # flip axis x and y
  coord_flip() +
  
  # theme
  theme(axis.line.x = element_line(colour = "white"), # empty theme except white axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=30), # increase axis title size
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15, color = "white"), # axis text in white
        axis.text.y = element_blank(), # remove axis text
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  
  scale_y_continuous(breaks=seq(0,10,1))
dens.1

# density plot top
dens.2 <- ggplot(data.df, aes(cvms_cvfs_1)) +
  
  # density
  geom_density(alpha=0.4, color = "dodgerblue2", fill = "dodgerblue2") +
  
  # data rug
  geom_rug(aes(x = SEC_1, y = 0), color = "dodgerblue2", position = position_jitter(width = 0.05,height = 0), alpha = 0.5) + # rug
  
  # axis titles
  labs(title="", x="", y = "Density") +
  
  # theme
  theme(axis.line.y = element_line(colour = "white"), # empty theme except white axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=30), # increase axis title size
        axis.text.x = element_blank(),  # remove axis text
        axis.text.y = element_text(size=15, color = "white"), # axis text in white
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  
  scale_y_continuous(breaks=seq(0,10,1))
dens.2

# blank panel 1
blank.1 <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# blank panel 2
blank.2 <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# blank panel 3
blank.3 <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# arrange
grid.arrange(dens.2, 
             blank.1, 
             blank.2, 
             scat.1, 
             blank.3, 
             dens.1, 
             ncol=3, nrow=2, 
             widths=c(4, 0.1, 0.8), heights=c(1.6, 4), 
             top=textGrob("",gp=gpar(fontsize=20,font=3)), 
             bottom=textGrob("",gp=gpar(fontsize=10,font=1), 
                             hjust = 1, x = 1)) # optionally add title and bottom text

dev.off()
setwd(dir = "/Users/amx/data_local/R/")
# ========== # ========== # ========== # ========== # ========== #