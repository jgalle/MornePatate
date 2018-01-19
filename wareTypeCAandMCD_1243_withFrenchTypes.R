 # set the working directory
#setwd("P:/DAACS/Archaeological Sites/Tennessee/The Hermitage/Chronology")

# wareTypeCAandMCD.R
# Establish a DBI connection to DAACS PostgreSQL database and submnit SQL queries
# Created by:  FDN  8.5.2014
# Last update: FDN 8.5.2014  
# Edited by:   LAB 1.17.2017 for Morne Patate
# Edited by:  LC 12.4.2017 hermitage phases
# Edited by:   FDN 12.21.2017 for tidyR


#load the library
require(RPostgreSQL)
library(dplyr)


# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
          dbname='daacs-production',
          user='drcquery', password='!queryacct!')


# get the table with the ware type date ranges
MCDTypeTable<- dbGetQuery(DRCcon,'
SELECT * 
FROM "tblCeramicWare"
     ')


# submit a SQL query: note the use of \ as an escape sequence
wareTypeData<-dbGetQuery(DRCcon,'
SELECT
"public"."tblCeramic"."Quantity",
"public"."tblCeramicWare"."Ware",
"public"."tblCeramicWare"."BeginDate",
"public"."tblCeramicWare"."EndDate",
"public"."tblCeramicCEWType"."CeramicCEWType",
"public"."tblProjectName"."ProjectName",
"public"."tblContext"."ProjectID",
"public"."tblContext"."Context",
"public"."tblContext"."DAACSStratigraphicGroup",
"public"."tblContext"."MasterContextNumber",
"public"."tblContext"."FeatureNumber",
"public"."tblContext"."QuadratID",
"public"."tblContext"."DAACSPhase"

FROM
"public"."tblProjectName"
INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"

WHERE                     
                        "public"."tblContext"."ProjectID" = \'1243\'
                          ')             


#summary1<-ddply(wareTypeData, .(DAACSPhase), summarise, Count=sum(Quantity))
#FDN do a summary using tidyR
summary1 <- wareTypeData %>% group_by(ProjectName,ProjectID) %>% summarise(count = sum(Quantity))
summary1


##Section 1: Customize the Manufacturing Dates ######################

# MCDTypeTable$BeginDate[MCDTypeTable$Ware == 'Porcelain, Chinese']<-1700
# MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Whiteware']<-1930
# MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Ironstone/White Granite']<-1930
# MCDTypeTable$EndDate[MCDTypeTable$Ware == 'Porcelain, French']<-1930

# FDN the tidyR version
# MCDTypeTable %>% 
#   mutate(BeginDate = replace(BeginDate, Ware == 'Porcelain, Chinese', 1700))
# MCDTypeTable %>% 
#   mutate(BeginDate = replace(BeginDate, Ware == 'Whiteware', 1930))
# MCDTypeTable %>% 
#   mutate(BeginDate = replace(BeginDate, Ware == 'Ironstone/White Granite', 1930))
# MCDTypeTable %>% 
#   mutate(BeginDate = replace(BeginDate, Ware ==  'Porcelain, French', 1930))
# 
# # FDN another tidyR version.
# # This one wrote the change into the orginal table.
# MCDTypeTable <- MCDTypeTable %>% 
#   mutate(BeginDate = replace(BeginDate, Ware == 'Porcelain, Chinese', 1700),
#          EndDate = replace(EndDate, Ware == 'Whiteware', 1930),
#          EndDate = replace(EndDate, Ware == 'Ironstone/White Granite', 1930),
#          EndDate = replace(EndDate, Ware ==  'Porcelain, French', 1930))
         
      
# Compute new numeric variables from original ones, which we will need to compute the MCDs
#Non-tidy version
# MCDTypeTable<-within(MCDTypeTable, {     # Notice that multiple vars can be changed
#   midPoint <- (EndDate+BeginDate)/2
#   span <- EndDate - BeginDate
#   inverseVar <- 1/(span/6)##2 
# })


# FDN the tidyR version
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/((EndDate - BeginDate)/6)##2 
         )

# Section 2:Create the UNIT Variable ######################
# This is the level at which assemblages are aggregated
# in the analysis

# delete contexts that have no phase assignment
# for MP we need to double check previous phases entered in DB
#wareTypeData <- filter( wareTypeData, !(DAACSPhase %in% c('',' ')) )

#sum(is.na(wareTypeData$DAACSPhase))

# create the Unit Field
#wareTypeData1 <-  mutate(wareTypeData, unit = paste(ProjectName,DAACSPhase))

wareTypeData$DAACSStratigraphicGroup[is.na(wareTypeData$DAACSStratigraphicGroup)] <- ''
wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- ''

wareTypeData1 <-
  mutate(wareTypeData, unit=ifelse((QuadratID == '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                    paste(Context),
                                    ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup == ''),
                                           paste(QuadratID,Context),
                                           ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup == ''),
                                                  paste(Context,FeatureNumber),
                                                  ifelse((QuadratID != '' & FeatureNumber == '' & DAACSStratigraphicGroup != ''),
                                                         paste(QuadratID,DAACSStratigraphicGroup),
                                                         ifelse((QuadratID == '' & FeatureNumber != '' & DAACSStratigraphicGroup != ''),
                                                                paste(FeatureNumber,DAACSStratigraphicGroup),
                                                                paste(Context)
                                                         ))))))

 
# FDN tidyR total type counts
summary2 <-wareTypeData1 %>% group_by(Ware) %>%  summarise(Count=sum(Quantity))
print(summary2, n=100)


## Section 3:Transpose the Data ######################

# Create new data frame with contexts as rows and type as cols, with the
# entries as counts

WareByUnit <- wareTypeData1 %>% group_by(Ware,unit) %>% summarise(count = sum(Quantity))

# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros
require(reshape2)
WareByUnitT <- dcast(WareByUnit, unit ~ Ware, value.var='count', fill=0 )

# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
WareByUnitTTotals<- rowSums(WareByUnitT[,2:ncol(WareByUnitT)])

# OK now let's get rid of all the rows where totals are <= 5
WareByUnitT1 <-WareByUnitT[WareByUnitTTotals>5,]  


##Section 4: Define an MCD function and Function to Remove Types w/o Dates ######################

# we build a function that remove types with no dates
# Two arguments 1. unitData: dataframe with counts of ware types in units
# the left variable IDs the units, while the rest of the varaibles are types
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 'inversevar'
# containing the manufacturing midpoints and inverse variances for the types.
# returns a list comprise of two dataframes:
# unitDataWithDates has units with types with dates
# typeDataWithDates has the types with dates

RemoveTypesNoDates <- function(unitData,typeData){
  
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTable
  typesWithNoDates <- typeData$Ware[(is.na(typeData$midPoint))]
  unitDataWithDates <- unitData[, ! colnames(unitData) %in%  typesWithNoDates]
  typeDataWithDates <- typeData[! typeData$Ware %in%  typesWithNoDates, ]
  unitDataWithDates <- filter(unitDataWithDates, rowSums(unitDataWithDates[,2:ncol(unitDataWithDates)]) > 0)
  return(list(unitDataWithDates=unitDataWithDates, typeDataWithDates=typeDataWithDates))
}
WareandTypeDatawithDates <- RemoveTypesNoDates(WareByUnitT1, MCDTypeTable)

# now we build a function that computes MCDs
# two arguments: 1. unitData: a dataframe with the counts of ware types in units. We assume
# the left variable IDs the units, while the rest of the varaibles are types
# 2. typeData: a dataframe with at least two variables named 'midPoint' and 'inversevar'
# containing the manufacturing midpoints and inverse variances for the types.
# returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they appeaed in the input
#     unitData dataframe.  

EstimateMCD<- function(unitData,typeData){
 #for debugging
 #unitData<- WareandTypeDatawithDates$unitDataWithDates
 #typeData <-WareandTypeDatawithDates$typeDataWithDates
 countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
 unitNames <- (unitData[,1])
 nUnits <- nrow(unitData)   
 nTypes<- nrow(typeData)
 nTypesFnd <-ncol(countMatrix)
 typeNames<- colnames(countMatrix)
 
 # create two col vectors to hold inverse variances and midpoints
 # _in the order in which the type variables occur in the data_.
 invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
 mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
   for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
     }
   }

 # compute the blue MCDs
 # get a unit by type matrix of inverse variances
 invVarMat<-matrix(t(invVar),nUnits,nTypesFnd, byrow=T)
 # a matrix of weights
 blueWtMat<- countMatrix * invVarMat
 # sums of the weight
 sumBlueWts <- rowSums(blueWtMat)
 # the BLUE MCDs
 blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
 # compute the vanilla MCDs
 sumWts <- rowSums(countMatrix)
 # the vanilla MCDs
 MCD<-(countMatrix %*% mPoint) / sumWts
 # Finally we assemble th results in to a list
 MCDs<-data.frame(unitNames,MCD,blueMCD,sumWts)
 colnames(MCDs)<- c('unit','MCD','blueMCD', 'Count' )
 midPoints <- data.frame(typeNames,mPoint)
 MCDs <- list(MCDs=MCDs,midPoints=midPoints)
 return(MCDs)
} 

#end of function EstimateMCD
 
# apply the function
MCDByUnit<-EstimateMCD(WareandTypeDatawithDates$unitDataWithDates,WareandTypeDatawithDates$typeDataWithDates)

# let's see what it looks like
MCDByUnit

# a function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. MCDs and midpoints)
# arguments:  the name of the variable that contains the unit scores (e.g. MCDs)
#             the name of the variable that contains the type score (e.g. the midpoints)
#             the name of the dataframe that contains the counts of ware types in units
# returns:    the sorted dataframe 
sortData<- function(unitScores,typeScores,unitData){
  #unitScores<-U3MCDByUnit$MCDs$blueMCD
  #typeScores<-U3MCDByUnit$midPoints$mPoint
  #unitData<- U3WareByUnitT1
  sortedData<-unitData[order(unitScores),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}


# apply the function
#WareByUnitT5Sorted<-sortData(MCDByUnit$MCDs$blueMCD,
 #                             MCDByUnit$midPoints$mPoint,
  #                           WareByUnitT2)

WareByUnitT2Sorted<-sortData(MCDByUnit$MCDs$blueMCD,
                            MCDByUnit$midPoints$mPoint,
                            WareandTypeDatawithDates$unitDataWithDates)

# now we prep the sorted dataframe to make a Ford-style battleship plot
# convert to a matrix, whose cols are the counts
# make the unit name a 'rowname" of the matrix
Mat<-as.matrix(WareByUnitT2Sorted[,2:ncol(WareByUnitT2Sorted)])
rownames(Mat)<-WareByUnitT2Sorted$unit
rSums<- matrix (rowSums(Mat),nrow(Mat),ncol(Mat), byrow=F)
MatProp<-Mat/rSums


#(package for seriation)
library(plotrix) 
battleship.plot(MatProp,
                     mar=c(2,5,5,1),
                     main = 'Seriation by Blue MCD',
                     xlab='Ware Type',
                     ylab= 'Context',
                     col='grey')

# dump out a CSV for a seriation plot using Lipo's Excel macro 
#write.csv(WareByUnitT2Sorted, file='DRC/WareByFeatureT1Sorted.csv')

##Section 5: Run the CA ######################

# now let's try some Correspondence Analysis
MatX<-as.matrix(WareByUnitT1[,2:ncol(WareByUnitT1)]) 
rownames(MatX)<-WareByUnitT1$unit

# This is the second function that we are now using as of 12/15 
require(ca)
ca3<-ca(MatX)

#head(ca3$rowcoord)
#head(ca3$colcoord)

#default plot
plot(ca3)

#str(ca3)

#summary(ca3)
plot(1:(length(ca3$sv)), ca3$sv^2 / sum(ca3$sv^2))
#ca3$sv

#create dataframe of unit/context dimension 1 and 2 scores for ggplot
rowscores <- data.frame(ca3$rowcoord[,1], ca3$rowcoord[,2])
colnames(rowscores) <- c("Dim1", "Dim2")

#create dataframe of ware type dimension 1 and 2 scores for ggplot
colscores <- data.frame(ca3$colcoord[,1], ca3$colcoord[,2])
colnames(colscores) <- c("Dim1", "Dim2")

#Create plot: Dim 1 and Dim 2 context scores
require(ggplot2)
library(ggrepel)
p1 <- ggplot(rowscores, aes(x=rowscores$Dim1,y=rowscores$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=rownames(rowscores)),vjust=-.6, hjust=-.1, cex=5)+
  #  xlim(-4,4)+
  #geom_text_repel(aes(label=rownames(rowscores)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="All Cabins", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p1
#ggsave("CA_AllCabin_Dim1Dim2.png", p1, width=10, height=7.5, dpi=300)


library(princurve)

plot(ca3$rowcoord[,c(1,2)])
guide <- locator()
guide <- rbind(guide$x,guide$y)

pc1 <- principal.curve(ca3$rowcoord[,c(1,2)],start=guide, maxit=100 )

lines(pc1$s[pc1$tag,])

whiskers <- function(from, to)
  segments(from[, 1], from[, 2], to[, 1], to[, 2])
whiskers(ca1$rowcoord[,c(1,2)], pc1$s)



# plot the row scores on dim1 and dim2
# plot(ca3$rowcoord[,1], ca3$rowcoord[,2], pch=2, bg="black", cex=1.25,
#      xlab="Dimension 1", ylab="Dimension 2")
# text(ca3$rowcoord[,1],ca3$rowcoord[,2],rownames(ca3$rowcoord),
#      pos=4, cex=.75, col="black")

p2 <- ggplot(colscores, aes(x=colscores$Dim1,y=colscores$Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_text(aes(label=rownames(colscores)),vjust=-.6, cex=5)+
  #geom_text_repel(aes(label=rownames(colscores)), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="All Cabins", x="Dimension 1", y="Dimension 2")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p2
#ggsave("CA_AllCabin_Dim1Dim2wares.png", p2, width=10, height=7.5, dpi=300)


# plot the col scores on dim1 and dim2, which types are important in which regions of the plot
# plot(ca3$colcoord[,1],ca3$colcoord[,2],pch=21, bg="black",cex=1.25,
#      xlab="Dimension 1", ylab="Dimension 2", asp=1 )
# text(ca3$colcoord[,1],ca3$colcoord[,2],rownames(ca3$colcoord),
#      pos=4 ,cex=.75, col="black")


p3 <- ggplot(rowscores, aes(x=rowscores$Dim1,y=MCDByUnit$MCDs$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=rownames(rowscores)),vjust=-.6, hjust=-.1, cex=5)+
  geom_text_repel(aes(label=rownames(rowscores)), cex=6, segment.alpha=0.2) +
  theme_classic()+
  scale_y_continuous(breaks=seq(1790, 1910, 10))+
  labs(title="All Cabins", x="Dimension 1", y="BLUE MCD")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p3 
#ggsave("CAbySitePhase_AllCabin_Dim1BlueMCD.png", p3, width=10, height=7.5, dpi=300)
# 
p3A <- ggplot(rowscores, aes(x=rowscores$Dim2,y=MCDByUnit$MCDs$blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=rownames(rowscores)),vjust=-.6, hjust=-.1, cex=5)+
  geom_text_repel(aes(label=rownames(rowscores)), cex=6, segment.alpha=0.2) +
  theme_classic()+
  scale_y_continuous(breaks=seq(1790, 1910, 10))+
  labs(title="All Cabins", x="Dimension 2", y="BLUE MCD")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p3A
ggsave("CAbySitePhase_AllCabin_Dim2BlueMCD.png", p3A, width=10, height=7.5, dpi=300)

# finally let's see what the relationship is between MCDs and CA scores

# # CA Dim 1 vs. MCDs
# plot(ca3$rowcoord[,1], MCDByUnit$MCDs$blueMCD, pch=21, bg="black",cex=1.25,
#      xlab="Dimension 1", ylab="BLUE MCD")
# text(ca3$rowcoord[,1],MCDByUnit$MCDs$blueMCD,rownames(ca3$rowcoord),
#      pos=4, cex=.75, col="black")
# 
# cor.test(ca3$rowcoord[,1],MCDByUnit$MCDs$blueMCD, method="kendall")


#Create weighted histogram for phasing
# library(plotrix)
# 
# weighted.hist(MCDByUnit$MCDs$blueMCD, MCDByUnit$MCDs$Count, breaks=seq(1750,1900,10), col='lightblue')
# 
# #Dim 1 Scores Weighted Histogram
# weighted.hist(ca3$rowcoord[,1], MCDByUnit$MCDs$Count,breaks=seq(-10,.25),col='blue', border='grey')
# 
# #Regular histogram with density curve
# hist(rep(ca3$rowcoord[,1], MCDByUnit$MCDs$Count),col='tan',border='darkgrey', 
#      breaks=seq(-7,3.8,.1),
#      main='',
#      xlab="Dimension 1 Scores",
#      freq=F)
# lines(density(ca3$rowcoord[,1], weights=MCDByUnit$MCDs$Count/sum(MCDByUnit$MCDs$Count)), 
#       lwd=2)

#Create table of contexts, counts, and mcds, need to read in unit as character
unit <- as.character(MCDByUnit$MCDs$unit)
dim1Scores <- ca3$rowcoord[,1]
dim2Scores <- ca3$rowcoord[,2]
MCD<- MCDByUnit$MCDs$MCD
blueMCD <-MCDByUnit$MCDs$blueMCD
count<- MCDByUnit$MCDs$Count

#Create data frame, read strings as characters
CA_MCD<-data.frame(unit, dim1Scores,dim2Scores,MCD,blueMCD, count, stringsAsFactors = F) 

p5 <- ggplot(CA_MCD, aes(x=CA_MCD$dim1Scores, weight=CA_MCD$count/sum(CA_MCD$count)))+
  geom_histogram(aes(y=..density..), colour="gray", fill="tan", binwidth=0.1, boundary=0.5)+
  #xlim(-3.5,7)+
  #stat_function(fun = dnorm, colour = "blue")+
  scale_x_continuous(breaks=seq(-4.5, 3.5, 0.5))+
  theme_classic()+
  labs(title="All Cabin Site Phases", x="Genre Dimension 1", y="Density")+
  theme(plot.title=element_text(size=rel(2.25), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)))
p5A <- p5 + geom_density(fill=NA) + geom_vline(xintercept=c(-3, -1, 2), colour="gray60")
p5A
#ggsave("CAbySitePhase_AllCabin_hist.png", p5A, width=10, height=7.5, dpi=300)
# 
 CA_MCD$SiteName <- CA_MCD$unit
 CA_MCD$SiteName[(grepl("^1406", CA_MCD$unit))] <- 'FQ'
 CA_MCD$SiteName[(grepl("^1401", CA_MCD$unit))] <- 'FQ'
 CA_MCD$SiteName[(grepl("^1405", CA_MCD$unit))] <- 'FQ'
 CA_MCD$SiteName[(grepl("^1407", CA_MCD$unit))] <- 'FQ'
 CA_MCD$SiteName[(grepl("^1403", CA_MCD$unit))] <- 'FQ'
 CA_MCD$SiteName[(grepl("^1400", CA_MCD$unit))] <- 'MBY'
 CA_MCD$SiteName[(grepl("^1404", CA_MCD$unit))] <- 'MBY'
 CA_MCD$SiteName[(grepl("^1402", CA_MCD$unit))] <- 'FH'
 CA_MCD$SiteName[(grepl("^1410", CA_MCD$unit))] <- 'FH'
 CA_MCD$SiteName[(grepl("^1412", CA_MCD$unit))] <- 'FH'
# 
#  CA_MCD$Site <- CA_MCD$unit
#  CA_MCD$Site[(grepl("^1406", CA_MCD$unit))] <- paste("Cab1", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1401", CA_MCD$unit))] <- paste("Cab2", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1405", CA_MCD$unit))] <- paste("Cab3", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1407", CA_MCD$unit))] <- paste("Cab4", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1403", CA_MCD$unit))] <- paste("KES", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1400", CA_MCD$unit))] <- paste("TX", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1404", CA_MCD$unit))] <- paste("Yard", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1402", CA_MCD$unit))] <- paste("South", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1410", CA_MCD$unit))] <- paste("East", CA_MCD$unit)
#  CA_MCD$Site[(grepl("^1412", CA_MCD$unit))] <- paste("West", CA_MCD$unit)
#  

library(stringr)
Name <- data.frame(str_split_fixed(CA_MCD$unit, " ", 2))
colnames(Name)<-c("Proj", "SPhase") 

CA_MCD$SiteName <- as.character(Name$Proj)
CA_MCD$SPhase <- Name$SPhase
CA_MCD$SiteName[(grepl("^1406", CA_MCD$unit))] <- 'Cab1'
CA_MCD$SiteName[(grepl("^1401", CA_MCD$unit))] <- 'Cab2'
CA_MCD$SiteName[(grepl("^1405", CA_MCD$unit))] <- 'Cab3'
CA_MCD$SiteName[(grepl("^1407", CA_MCD$unit))] <- 'Cab4'
CA_MCD$SiteName[(grepl("^1403", CA_MCD$unit))] <- 'KES'
CA_MCD$SiteName[(grepl("^1400", CA_MCD$unit))] <- 'TPX'
CA_MCD$SiteName[(grepl("^1404", CA_MCD$unit))] <- 'Yard'
CA_MCD$SiteName[(grepl("^1402", CA_MCD$unit))] <- 'South'
CA_MCD$SiteName[(grepl("^1410", CA_MCD$unit))] <- 'East'
CA_MCD$SiteName[(grepl("^1412", CA_MCD$unit))] <- 'West'

CA_MCD$SiteName2 <- paste(CA_MCD$SiteName, CA_MCD$SPhase)

#Fix labels - add site name
#color <- ggplot(CA_MCD, aes(x=CA_MCD$dim1Scores,y=CA_MCD$blueMCD))+
 color <- ggplot(CA_MCD, aes(x=CA_MCD$dim1Scores,y=CA_MCD$dim2Scores))+
  geom_point(aes(fill=CA_MCD$SiteName), shape=21, size=5, alpha=0.75)+
  #geom_point(aes(colour=CA_MCD$Cabin),size=5)+
  #geom_text(aes(label=CA_MCD_Phase1$unit),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=CA_MCD$SiteName2), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="All Cabins", x="Dimension 1", y="BLUE MCD")+
  #scale_y_continuous(breaks=seq(1790, 1900, 10))+
  theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.75)),
        legend.title=element_text(size=rel(1.5)), legend.position="bottom")+
  scale_fill_manual(name="Location", values=c("darkblue", "green", "deeppink"))
color
#ggsave("CAbySitePhase_AllCabin_Dim1MCDbySite.png", color, width=10, height=7.5, dpi=300)

# now we prep the sorted dataframe to make a Bertin plot
Scores <- data.frame(ca3$rowcoord[,1], ca3$colcoord[,1])
colnames(colscores) <- c("Dim1", "Dim2")

ScoreByUnit2<-sortData(ca3$rowcoord[,1],
                             ca3$colcoord[,1],
                       WareByUnitT2)

ScoreByUnit3 <- ScoreByUnit2[,2:ncol(ScoreByUnit2)]
Mat2<-as.matrix(ScoreByUnit3[,2:ncol(ScoreByUnit3)])

rownames(Mat2)<-ScoreByUnit2$unit
rSums<- matrix (rowSums(Mat2),nrow(Mat2),ncol(Mat2), byrow=F)
MatProp<-Mat2/rSums

#do the plot
#library(seriation)
#bertinplot(MatProp, highlight=F)

#(package for seriation)
library(plotrix)
battleship.plot(MatProp,
                mar=c(2,5,5,1),
                #main = 'Seriation',
                #xlab='ManuTech',
                ylab= 'Context',
                col='grey')
# 
# 

# create a vector for the phases with as many entries as assemblages
Phase <- rep(NA, length(ca3$rowcoord[,1])) 

# do the phase assigments
Phase[(ca3$rowcoord[,1] <= -3)] <- 'P01'
Phase[(ca3$rowcoord[,1] >-3 & (ca3$rowcoord[,1]) < -1)] <- 'P02'
Phase[(ca3$rowcoord[,1] >-1 & (ca3$rowcoord[,1]) < 2)] <- 'P03'
Phase[(ca3$rowcoord[,1] > 2)] <- 'P04'

#create table of contexts, counts, and mcds
unit <- MCDByUnit$MCDs$unit
dim1Scores <- ca3$rowcoord[,1]
dim2Scores <- ca3$rowcoord[,2]
MCD<- MCDByUnit$MCDs$MCD
blueMCD <-MCDByUnit$MCDs$blueMCD
count<- MCDByUnit$MCDs$Count

CA_MCD_Phase<-data.frame(unit, dim1Scores,dim2Scores,MCD,blueMCD, Phase, count) 

#Order by dim1 score
CA_MCD_Phase <- CA_MCD_Phase[order(CA_MCD_Phase$dim1Scores),]

#weighted mean
#tapply function = applies whatever function you give it, x is object on which you calculate the function
#W is numerical weighted vector
tapply(CA_MCD_Phase$blueMCD, CA_MCD_Phase$Phase, weighted.mean)


color2 <- ggplot(CA_MCD_Phase, aes(x=CA_MCD_Phase$dim1Scores,y=CA_MCD_Phase$blueMCD))+
  geom_point(aes(fill=CA_MCD_Phase$Phase), shape=21, size=5)+
  #geom_point(aes(colour=CA_MCD$Cabin),size=5)+
  #geom_text(aes(label=CA_MCD_Phase$unit),vjust=-.6, cex=5)+
  geom_text_repel(aes(label=CA_MCD_Phase$unit), cex=5, segment.alpha=0.2) +
  theme_classic()+
  labs(title="All Cabins", x="Dimension 1", y="BLUE MCD")+
  scale_y_continuous(breaks=seq(1790, 1900, 10))+
  theme(plot.title=element_text(size=rel(2), hjust=0.5),axis.title=element_text(size=rel(1.75)),
        axis.text=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.75)),
        legend.title=element_text(size=rel(1.5)), legend.position="bottom")+
  scale_fill_manual(name="Location",
                    values=c("skyblue", "dodgerblue", "blue", "darkblue"))
color2
#ggsave("CAbySitePhase_AllCabin_Dim1MCDbyPhase.png", color2, width=10, height=7.5, dpi=300)

write.csv(CA_MCD_Phase, "CA_MCD_Phase.csv")

WareByPhaseX <- ddply(CA_MCD_Phase, "Phase", numcolwise(sum))



#Once phases are assigned we need to calculate MCDs and TPQs by phase 
#Add phase assignments to ware counts by unit
WareByUnitT3Sorted <- merge(WareByUnitT2Sorted, CA_MCD_Phase, by='unit')
#get rid of dimscores and MCD columns
WareByUnitT4Sorted <- WareByUnitT3Sorted[-c(17:22)]

#aggregate counts for ware type by phase
WareByPhase <- ddply(WareByUnitT4Sorted, "Phase", numcolwise(sum))
#Check ware by phase
WareByPhase


#alter EstimateMCDandTPQ function to have phaseData as input for unitData
EstimateMCDandTPQ<- function(phaseData,typeData){
  #for debugging
  #phaseData<- WareByPhase
  #typeData <- MCDTypeTable     
  countMatrix<- as.matrix(phaseData[,2:ncol(phaseData)])
  phaseNames <- (phaseData[,1])
  nPhases <- nrow(phaseData)   
  nTypes<- nrow(typeData)
  nTypesFnd <-ncol(countMatrix)
  typeNames<- colnames(countMatrix)
  # create two col vectors to hold inverse variances and midpoints
  # _in the order in which the type variables occur in the data_.
  invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
  mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
    for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
    }
  }
  # replace NAs for types with no dates with 0s -- so they do not count
  # compute the blue MCDs
  # get a unit by type matrix of inverse variances
  invVarMat<-matrix(t(invVar),nPhases,nTypesFnd, byrow=T)
  # a matrix of weights
  blueWtMat<- countMatrix * invVarMat
  # sums of the weight
  sumBlueWts <- rowSums(blueWtMat)
  # the BLUE MCDs
  blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
  # compute the vanilla MCDs
  sumWts <- rowSums(countMatrix)
  # the vanilla MCDs
  MCD<-(countMatrix %*% mPoint) / sumWts
  # now for the TPQs
  meltedPhaseData<- melt(phaseData, id.vars='Phase',  variable.name = 'Ware', value.name='count')
  meltedPhaseData1 <- subset(meltedPhaseData, count > 0) 
  mergedPhaseData <- merge(x = meltedPhaseData1, y = typeData,  by.x='Ware', by.y='Ware')
  # the trick is that to figure out the tpq it's best to have each record (row) represent an individual sherd
  # but in its current state, each record has a count that is likely more than 1 so it's necessary to break them up
  # use rep and rownames - rowname is a unique number for each row, kind of link an index
  # rep goes through dataframe mergedUnitData and replicates based on the count column, i.e. if count is
  # 5 it will create 5 records or rows and only replicates columns 2 and 6 (2 is unit name and 6 is begin date)
  repPhaseData <- mergedPhaseData[rep(rownames(mergedPhaseData),mergedPhaseData$count),c(2,6)]
  #once all the rows have a count of one, then can run the quantile function
  TPQ <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repPhaseData$BeginDate,repPhaseData$Phase, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(phaseNames,MCD,blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c('Phase','MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 

#end of function EstimateMCD

# apply the function
MCDByPhase<-EstimateMCDandTPQ(WareByPhase,MCDTypeTable)

# let's see what it looks like
MCDByPhase

#check sums of counts for phases
ddply(CA_MCD_Phase, .(Phase), summarise, Count=sum(count))

#weighted mean
#tapply function = applies whatever function you give it, x is object on which you calculate the function
#W is numerical weighted vector
tapply(CA_MCD_Phase$blueMCD, CA_MCD_Phase$Phase, weighted.mean)



#####Section 6: Context Phases #####################

#Once phases are assigned we need to have a list of Phases by context to update the database 
#Creat context-level dataframe for the project including Context, Feature Number, and SGs
#ContextListwareTypeDataX<-dbGetQuery(DRCcon,'
ContextList<-dbGetQuery(DRCcon,'
SELECT
"public"."tblCeramic"."Quantity",
"public"."tblCeramicWare"."Ware",
"public"."tblCeramicWare"."BeginDate",
"public"."tblCeramicWare"."EndDate",
"public"."tblContext"."ProjectID",
"public"."tblContext"."Context",
"public"."tblContext"."DAACSPhase"

FROM
"public"."tblContext"
INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"

WHERE                     
"public"."tblContext"."ProjectID" = \'1400\'OR
"public"."tblContext"."ProjectID" = \'1401\'OR
"public"."tblContext"."ProjectID" = \'1403\'OR
"public"."tblContext"."ProjectID" = \'1404\'OR
"public"."tblContext"."ProjectID" = \'1402\'OR
"public"."tblContext"."ProjectID" = \'1410\'OR
"public"."tblContext"."ProjectID" = \'1412\'OR
"public"."tblContext"."ProjectID" = \'1406\'OR
"public"."tblContext"."ProjectID" = \'1407\'OR
"public"."tblContext"."ProjectID" = \'1405\'
')             


ContextList$DAACSPhase[ContextList$DAACSPhase == ''] <- NA
ContextList$DAACSPhase[ContextList$DAACSPhase == ' '] <- NA

ContextList2<- subset(ContextList,! is.na(ContextList$DAACSPhase))

ContextList2$unit <- paste(ContextList2$ProjectID, ContextList2$DAACSPhase) 

#Merge ContextList and CA_MCD_Phase by unit field
ContextPhases <- merge(ContextList2, CA_MCD_Phase, by.x="unit", by.y="unit", all=T)

#Remove unnecessary columns
ContextPhases2 <- ContextPhases[, c(10,21)]

UniqueSouthCabin<- unique(ContextPhases2)

#Create csv list of contexts and phase assignments, label by site name
write.csv(ContextPhases, file='ContextPhases_SouthCabin.csv')



