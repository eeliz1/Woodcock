###################################
###MCP vegetation 
###################################


setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/")

veg_files = Sys.glob("*.csv")

##writes each file back out with an MCP_ID 
##column included
for (file in veg_files){
  df = read.csv(file)
  df$MCP_ID = paste(substr(file, 0, nchar(x) -4), df$SamplePos[1], sep="_")
  write.csv(df, file = paste("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/MCP_veg/",
                             df$MCP_ID[1], ".csv", sep=""))
}

setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/MCP_veg")

MCP_veg_files = Sys.glob("*.csv")
MCP_veg_files
MCP_comb_file=ldply(MCP_veg_files, read.csv)

head(MCP_comb_file)

tail(MCP_comb_file)

MCF = subset(MCP_comb_file, SamplePos=="MCP")
MCF
MCPs <- sort(unique(gsub(".........._.....", "\\1", MCP_comb_file$MCP_ID)))
MCPs




