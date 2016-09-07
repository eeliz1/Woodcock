#Example Code for Lisa
library(plyr)

#Set working directory
setwd("C:/Users/Elisa/Documents/Woodcock/R_Woodcock/Veg_csv")
      
      #this tells you what files are in the working directory folder then slices the list
      filenames=list.files()
      x = filenames[9:10]

      x
      #This combines all thefiles you created together into a object called 
      comb.files=ldply(x, read.csv)
      str(comb.files)
      
      #this writes all the combined files into a single file called comb_woodcock
      write.csv(comb.files, 
      "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/Veg_csv/date/171368602_Jan12_2016_all.csv")
