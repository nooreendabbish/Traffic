library(zipcode)
data(zipcode)
head(zipcode)

load("GES2014.rda")
GES2014$DROWSY <- ifelse(GES2014$DRIMPAIR==2, 1, 0)
GES2014$DR_ZIP <- as.character(ifelse(GES2014$DR_ZIP %in% c(00000, 99997, 99999), NA, GES2014$DR_ZIP))
GES2014$DR_ZIP <- ifelse(nchar(GES2014$DR_ZIP) == 3, paste0("00", GES2014$DR_ZIP), 
                         ifelse(nchar(GES2014$DR_ZIP) == 4, paste0("0", GES2014$DR_ZIP), GES2014$DR_ZIP))
GES2014.sub <- subset(GES2014, select = c("WEIGHT", "DROWSY", "DR_ZIP"))
GES2014.sub <- na.omit(GES2014.sub) ##Running this before the loop will save time

##Zip code 60676 appears in GES2014 but not zipcode -- let's write an ifelse statement to account for NA's
                      
GES2014.sub$DR_STATE <- rep("", nrow(GES2014.sub))
for (row in 1:nrow(GES2014.sub))
{
  GES2014.sub$DR_STATE[row] <- ifelse(length(which(zipcode$zip == GES2014.sub$DR_ZIP[row])) > 0,
                                      zipcode$state[which(zipcode$zip == GES2014.sub$DR_ZIP[row])],
                                      NA)
  if (row %% 1000 == 0)
    print(row)
}

GES2014.sub <- na.omit(GES2014.sub)
GES2014.states <- GES2014.sub
save(GES2014.states, file="GES2014.states.rda")