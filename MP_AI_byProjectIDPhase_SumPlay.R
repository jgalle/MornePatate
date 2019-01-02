# Calculates AI values for artifact types
# MP Block C quad but not STPs removed
# Denominators are Green Bottle Glass sum for all contexts except Block C quad
# and Coarse EW (imported and local) ditto
#first draft Lynsey Bates 2017
#revised Jg 1/2/2019

# setwd
setwd("C:/temp")

#load the library
require(RPostgreSQL)
library(dplyr)
require(tidyr)


# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname='daacs-production',
                  user='drcquery', password='!queryacct!')

### A. Get Glass Totals ###############
Glass <-dbGetQuery(DRCcon,'
SELECT
                  "public"."tblContext"."ProjectID",
                  "public"."tblContext"."DAACSPhase",
                  "public"."tblContext"."MasterContextNumber",
                  "public"."tblGlass"."Quantity" as "WBGSum",
                  "public"."tblGlassForm"."GlassForm",
                  "public"."tblBasicColor"."BasicColor"

FROM
                  "public"."tblContext"
                  INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                  INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                  INNER JOIN "public"."tblGlass" ON "public"."tblGlass"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                  INNER JOIN "public"."tblGlassForm" ON "public"."tblGlassForm"."GlassFormID" = "public"."tblGlass"."GlassFormID"
                  INNER JOIN "public"."tblBasicColor" ON "public"."tblGlass"."GlassBasicColorID" = "public"."tblBasicColor"."BasicColorID"
                  WHERE
                  "public"."tblContext"."ProjectID" = \'1243\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1250\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1251\'
                         
                         ')    

# Filter by only green color 
Glass <- Glass %>% filter(BasicColor =='Green/Olive Green')

# Remove non-bottle forms
Glass <- Glass %>% filter(!GlassForm %in% c('Flask', 'Unidentifiable','Not Recorded'))

# Summarize by project
GlassSum <- Glass %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise_at("WBGSum", sum)



###C. Gen Arts ##############

GenArts <-dbGetQuery(DRCcon,'
                 SELECT
                 "public"."tblContext"."ProjectID",
                 "public"."tblGenArtifact"."Quantity",
                 "public"."tblGenArtifactForm"."GenArtifactForm",
                 "public"."tblContext"."DAACSPhase",
                  "public"."tblContext"."MasterContextNumber",
                 "public"."tblContext"."Context"
                 FROM
                 "public"."tblContext"
                 INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                 INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                 INNER JOIN "public"."tblProject" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"                       
                 INNER JOIN "public"."tblProjectName" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"                       
                 INNER JOIN "public"."tblGenArtifact" ON "public"."tblGenArtifact"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                 INNER JOIN "public"."tblGenArtifactForm" ON "public"."tblGenArtifactForm"."GenArtifactFormID" = "public"."tblGenArtifact"."GenArtifactFormID"
                 WHERE                     
                 "public"."tblContext"."ProjectID" = \'1243\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1250\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1251\'

                 ') 


# Summarize Forms by ProjectID, DAACS Phase
GenArtsSum <-  GenArts %>% group_by(DAACSPhase, MasterContextNumber, GenArtifactForm) %>% 
  summarise(count = sum(Quantity))

###C1. Category Assignments ##############

#Assign categories to different gen art forms
GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Doll, Frozen Charlotte',
                                                        'Doll, head','Doll, limb', 'Doll, other', 
                                                        'Doll, eye', 'Gaming Piece', 'Gaming Piece, die',
                                                        'Domino', 'Marble, toy','Toy, car','Toy, cannon',
                                                        'Toy, dish','Toy, figurine','Toy, other'), 'Toy_Game', GenArtifactForm))

GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Figurine','Plate, trunk','Chandelier/Epergne Pendant',
                                                        'Tack, antimacassar'),
                                 'DecorativeHome', Category))

GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Fan Blade/Part','Spectacles','Eye Glass','Aiglet',
                                                        'Crinoline, clamp','Eye, clothing','Rivet, clothing',
                                                        'Fastener, clothing','Purse Part, clasp','Purse Part, mesh',
                                                        'Fastener, corset','Parasol/Umbrella, other',
                                                        'Parasol/Umbrella, stretcher/rib','Hook, clothing',
                                                        'Rivet, clothing', 'Hair Clasp','Suspender Brace',
                                                        'Suspender, hook',
                                                        'Shoe Sole','Shoe Upper','Shoe, tip','Shoe, tap',
                                                        'Shoe, heel','Rivet, shoe',
                                                        'Brooch','Chain, watch','Charm, hand','Cuff Link','Jewel',
                                                        'Jewelry, earring','Jewelry, other','Jewelry, pendant',
                                                        'Jewelry, Pin', 'Medal, religious', 'Pendant',
                                                        'Ring, jewelry', 'Clock/Watch part','Key, watch','Watch Gear',
                                                        'Watch Part'), 
                                 'Adornment', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Slate, writing', 'Pencil, slate'), 
                                          'Education', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Musical Instrument, key',
                                                                 'Musical Instrument, unidentifiable',
                                                                 'Harmonica Plate and Reed',
                                                                 'Harmonica Reed','Accordion Plate and Reed',
                                                                 'Accordion Plate','Harmonica Plate','Jews/Jaw Harp',
                                                                 'Musical Instrument'), 
                                          'Music', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Gun Part, unidentified',
                                                                 'Gun, hammer',
                                                                 'Gun, plate',
                                                                 'Flask, powder','Gunflint','Fish Hook',
                                                                 'Weight, Net'), 
                                          'FoodProcurement', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Comb, folding',
                                                                 'Comb, hair',
                                                                 'Comb, nit/lice','Curling Iron','Toothbrush',
                                                                 'Razor'), 
                                          'Hygiene', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Needle','Needle Case','Pin, straight',
                                                                 'Scissors','Thimble',
                                                                 'Blank, button','Bodkin','Bobbin, lace',
                                                                 'Darning Egg','Hook, crochet','Hook, tambour',
                                                                 'Knitting Needle Guards','Needle, mattress','Thread Spool',
                                                                 'Sewing Equipment, unidentified'), 
                                          'Sewing', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Fork, toasting'), 
                                          'Utensil', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Horse Furniture','Stirrup','Spur'), 
                                          'Horse', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Plate, printing'), 
                                          'Social', Category))

# Create dataframe by block and category
GenArtSum<-GenArtsSum %>% group_by(DAACSPhase, MasterContextNumber, Category) %>%  summarise(count = sum(count))

# Filter by Categories created above
GenArtSum2 <- filter(GenArtSum, Category %in% c('Adornment', 'Hygiene',
                                            'Medical','Utensil', 'DecorativeHome',
                                            'Sewing', 'Horse', 'Music', 
                                            'GunParts_Ammunition_FoodProcurement',
                                            'Social', 'Education',
                                            'Toy_Game'))

### D. Pipes ######################
TobPipes <-dbGetQuery(DRCcon,'
                    SELECT
                    "public"."tblContext"."ProjectID",
                    "public"."tblTobaccoPipe"."Quantity",
                    "public"."tblContext"."DAACSPhase",
                    "public"."tblContext"."MasterContextNumber",
                    "public"."tblContext"."Context"
                    FROM
                    "public"."tblContext"
                    INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                    INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                    INNER JOIN "public"."tblTobaccoPipe" ON "public"."tblTobaccoPipe"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                    WHERE
                    "public"."tblContext"."ProjectID" = \'1243\'

                     OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                    ') 

# Summarize by ProjectID, DACSPHase
TobPipeSum <-  TobPipes %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Add Category
TobPipeSum$Category <- 'Pipe'

### E. Beads, Buckles, Buttons ############

### E1. Beads ############

Beads<-dbGetQuery(DRCcon,'
                   SELECT
                   "public"."tblContext"."ProjectID",
                   "public"."tblBeadMaterial"."BeadMaterial",
                   "public"."tblContext"."DAACSPhase",
                  "public"."tblContext"."MasterContextNumber",
                   "public"."tblBead"."Quantity"
                   FROM
                   "public"."tblContext"
                   INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                   INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                   INNER JOIN "public"."tblBead" ON "public"."tblBead"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                   INNER JOIN "public"."tblBeadMaterial" ON "public"."tblBead"."BeadMaterialID" = "public"."tblBeadMaterial"."BeadMaterialID"
                   WHERE
                   "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                     OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                   ') 

# Summarize by ProjctID, DAACSPHase
BeadSum <-  Beads %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Add category assignment
BeadSum$Category <- 'Adornment'

###E2. Buttons############

Buttons<-dbGetQuery(DRCcon,'
                     SELECT
                     "public"."tblContext"."ProjectID",
                     "public"."tblButtonMaterial"."ButtonMaterial",
                     "public"."tblButton"."Quantity",
                     "public"."tblContext"."DAACSPhase",
                     "public"."tblContext"."MasterContextNumber",
                     "public"."tblContext"."Context"
                     FROM
                     "public"."tblContext"
                     INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                     INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                     INNER JOIN "public"."tblButton" ON "public"."tblButton"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                     INNER JOIN "public"."tblButtonMaterial" ON "public"."tblButton"."ButtonMaterialID" = "public"."tblButtonMaterial"."ButtonMaterialID"
                     WHERE
                     "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                     ') 


# Summarize by ProjectID, DAACSPhase 
ButtonSUM <-  Buttons %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Add category assignment
ButtonSUM$Category <- 'Adornment'

###E3. Buckle############
Buckles<-dbGetQuery(DRCcon,'
                     SELECT
                     "public"."tblContext"."ProjectID",
                     "public"."tblBuckle"."Quantity",
                     "public"."tblBuckleType"."BuckleType",
                     "public"."tblContext"."DAACSPhase",
                     "public"."tblContext"."MasterContextNumber",
                     "public"."tblContext"."Context"
                     FROM
                     "public"."tblContext"
                     INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                     INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                     INNER JOIN "public"."tblBuckle" ON "public"."tblBuckle"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                     INNER JOIN "public"."tblBuckleType" ON "public"."tblBuckle"."BuckleTypeID" = "public"."tblBuckleType"."BuckleTypeID"
                     WHERE
                     "public"."tblContext"."ProjectID" = \'1243\'

                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                     ') 
#Summarize by ProjectID, DAACSPhase
BuckleSUM <-  Buckles %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Assign category
BuckleSUM$Category <- 'Adornment'

###F. Utensils #############

Utensils <-dbGetQuery(DRCcon,'
                    SELECT
                    "public"."tblContext"."ProjectID",
                    "public"."tblContext"."Context",
                    "public"."tblUtensil"."Quantity",
                    "public"."tblContext"."DAACSPhase",
                    "public"."tblContext"."MasterContextNumber"
                    FROM
                    "public"."tblContext"
                    INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                    INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                    INNER JOIN "public"."tblUtensil" ON "public"."tblUtensil"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                    WHERE
                    "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                      "public"."tblContext"."ProjectID" = \'1250\'
                      OR
                      "public"."tblContext"."ProjectID" = \'1251\'
                    ') 

# Summarize by ProjectID, DAACSPhase
UtensilSum <-  Utensils %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Assign category
UtensilSum$Category <- 'Utensil'

###G. Ceramic Discs #############

Ceramic<-dbGetQuery(DRCcon,'
SELECT
                         "public"."tblCeramic"."Quantity",
                         "public"."tblCeramicWare"."Ware",
                         "public"."tblCeramicCEWType"."CeramicCEWType",
                         "public"."tblContext"."ProjectID",
                         "public"."tblContext"."DAACSPhase",
                         "public"."tblContext"."MasterContextNumber",
                          "public"."tblCeramicForm"."CeramicForm",
                          "public"."tblCeramicMaterial"."CeramicMaterial"
                         FROM
                         "public"."tblProjectName"
                         INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
                         INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
                         INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                         INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                         INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                         INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                         LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"
INNER JOIN "public"."tblCeramicForm" ON "public"."tblCeramic"."CeramicFormID" = "public"."tblCeramicForm"."CeramicFormID"
INNER JOIN "public"."tblCeramicMaterial" ON "public"."tblCeramic"."CeramicMaterialID" = "public"."tblCeramicMaterial"."CeramicMaterialID"
                         WHERE
                         "public"."tblContext"."ProjectID" = \'1243\'
                          OR
                         "public"."tblContext"."ProjectID" = \'1250\'
                           OR
                          "public"."tblContext"."ProjectID" = \'1251\'
                          ')

### G2. Ceramic Gaming Discs ##########

# Filter by Form
CeramicDiscs <- Ceramic %>% filter(grepl('Gaming',CeramicForm))

# Summarize by ProjectID, DAACSPhase
CeramicDiscSUM <-  CeramicDiscs %>% group_by(DAACSPhase, MasterContextNumber) %>% summarise(count = sum(Quantity))

# Assign category
CeramicDiscSUM$Category <- 'CeramicDisc'

## H. One Table ######################

# Bind together all artifact category tables
smallFinds <- bind_rows(TobPipeSum, UtensilSum, GenArtSum2, BuckleSUM, BeadSum, ButtonSUM, CeramicDiscSUM)

library(reshape2)
SmallFindsMelt <- melt(smallFinds, id=(c("DAACSPhase","MasterContextNumber")))



# SmallFinds <- union(Pharm2, GenArts2) 
# SmallFinds <- union(SmallFinds, Coins)
# SmallFinds <- union(SmallFinds, Buckle)
# SmallFinds <- union(SmallFinds, Bead)
# SmallFinds <- union(SmallFinds, Button)

# Summarize file by Category
smallFindSum <- smallFinds %>% group_by(DAACSPhase, MasterContextNumber, Category) %>%  summarise(count = sum(count))

# Add missing values
smallFindSumZeros <- smallFindSum %>% spread(Block,count,fill=0)

# Read in WBG and CEW counts
wbgCEW <- read.csv("WBG_CEW_countsbyblock.csv", header=T, stringsAsFactors = F)              


## I. Adornment ######################

adornment <- filter(smallFindSum, Category == 'Adornment')

# Join with WBG and Adorn totals
(adorn <- left_join(GlassSum, adornment)) # do not need a by statement here. Asking for everything in GlassSum and matching to AdornLeft Table



# Replace NAs
adorn <- adorn %>% mutate(Category=ifelse(is.na(Category), 'Adornment', Category))

adorn <- adorn %>% mutate(count=ifelse(is.na(count), 0, count))

# Calculate AIs
adorn <- adorn %>% mutate(adCEW=count+CEW)
adorn <- adorn %>% mutate(cewAI=count/(count+CEW))

adorn <- adorn %>% mutate(adWBG=count+WBG)
adorn <- adorn %>% mutate(wbgAI=count/(count+WBG))

adjustedWaldCI<-function(count,total,alpha){
  nTilde <- total+4
  pTilde <- (count+2)/(total+4)
  se <- sqrt((pTilde*(1-pTilde))/(nTilde))
  upperCL <- pTilde + se * qnorm(1-(alpha/2))
  lowerCL <- pTilde + se * qnorm(alpha/2) 
  upperCL<-ifelse ( upperCL > 1, 1, upperCL)
  lowerCL <-ifelse ( lowerCL < 0, 0, lowerCL)                               
  return(data.frame(pTilde,upperCL,lowerCL))
}

#run function on form data
wbgCI <- adjustedWaldCI(adorn$count,adorn$adWBG,0.05)
adorn$gCIUpper <- wbgCI$upperCL
adorn$gCILower <- wbgCI$lowerCL
adorn$gp <- wbgCI$pTilde

adorn <- adorn %>% mutate(gp=ifelse(count==0, 0, gp))
adorn <- adorn %>% mutate(gCIUpper=ifelse(count==0, 0, gCIUpper))

set.seed(42)
a<-ggplot(adorn, aes(x=adorn$Block, y=adorn$gp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = adorn$gCILower,ymax = adorn$gCIUpper), color="black", width=0.5) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Adornment/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
#  scale_x_continuous(limits=c(1670,1850)) +
#  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
  #scale_fill_manual(name="State", values=c("blue", "orange"))
a
ggsave("MPAI_ByBlock_AdornWBG.png", a, width=10, height=7.5, dpi=300)


#run function on data
cewCI <- adjustedWaldCI(adorn$count,adorn$adCEW,0.05)
adorn$cCIUpper <- cewCI$upperCL
adorn$cCILower <- cewCI$lowerCL
adorn$cp <- cewCI$pTilde

adorn <- adorn %>% mutate(cp=ifelse(count==0, 0, cp))
adorn <- adorn %>% mutate(cCIUpper=ifelse(count==0, 0, cCIUpper))

set.seed(42)
b<-ggplot(adorn, aes(x=adorn$Block, y=adorn$cp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = adorn$cCILower,ymax = adorn$cCIUpper), color="black", width=0.25) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Adornment/CEW Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
b
ggsave("MPAI_ByBlock_AdornCEW.png", b, width=10, height=7.5, dpi=300)


## J. Pipe ######################
pipes <- filter(smallFinds, Category == 'Pipe')

# Join with WBG and CEW totals
pipes <- left_join(wbgCEW, pipes, by='Block')

# Replace NAs
pipes <- pipes %>% mutate(Category=ifelse(is.na(Category), 'Pipe', Category))

pipes <- pipes %>% mutate(count=ifelse(is.na(count), 0, count))

# Calculate AIs
pipes <- pipes %>% mutate(tpCEW=count+CEW)
pipes <- pipes %>% mutate(cewAI=count/(count+CEW))

pipes <- pipes %>% mutate(tpWBG=count+WBG)
pipes <- pipes %>% mutate(wbgAI=count/(count+WBG))

#run function on form data
wbgCI <- adjustedWaldCI(pipes$count,pipes$tpWBG,0.05)
pipes$gCIUpper <- wbgCI$upperCL
pipes$gCILower <- wbgCI$lowerCL
pipes$gp <- wbgCI$pTilde

pipes <- pipes %>% mutate(gp=ifelse(count==0, 0, gp))
pipes <- pipes %>% mutate(gCIUpper=ifelse(count==0, 0, gCIUpper))

set.seed(42)
c<-ggplot(pipes, aes(x=pipes$Block, y=pipes$gp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = pipes$gCILower,ymax = pipes$gCIUpper), color="black", width=0.5) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Tobacco Pipes/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.7), breaks=seq(0, 0.7, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
c
ggsave("MPAI_ByBlock_PipesWBG.png", c, width=10, height=7.5, dpi=300)


#run function on data
cewCI <- adjustedWaldCI(pipes$count,pipes$tpCEW,0.05)
pipes$cCIUpper <- cewCI$upperCL
pipes$cCILower <- cewCI$lowerCL
pipes$cp <- cewCI$pTilde

pipes <- pipes %>% mutate(cp=ifelse(count==0, 0, cp))
pipes <- pipes %>% mutate(cCIUpper=ifelse(count==0, 0, cCIUpper))

set.seed(42)
d<-ggplot(pipes, aes(x=pipes$Block, y=pipes$cp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = pipes$cCILower,ymax = pipes$cCIUpper), color="black", width=0.25) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Pipes/CEW Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.7), breaks=seq(0, 0.7, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
d
ggsave("MPAI_ByBlock_PipesCEW.png", d, width=10, height=7.5, dpi=300)


## K. Discs ######################
discs <- filter(smallFinds, Category == 'GameDisc')

# Join with WBG and CEW totals
discs <- left_join(wbgCEW, discs, by='Block')

# Replace NAs
discs <- discs %>% mutate(Category=ifelse(is.na(Category), 'GameDisc', Category))

discs <- discs %>% mutate(count=ifelse(is.na(count), 0, count))

# Calculate AIs
discs <- discs %>% mutate(gdCEW=count+CEW)
discs <- discs %>% mutate(cewAI=count/(count+CEW))

discs <- discs %>% mutate(gdWBG=count+WBG)
discs <- discs %>% mutate(wbgAI=count/(count+WBG))

#run function on form data
wbgCI <- adjustedWaldCI(discs$count,discs$gdWBG,0.05)
discs$gCIUpper <- wbgCI$upperCL
discs$gCILower <- wbgCI$lowerCL
discs$gp <- wbgCI$pTilde

discs <- discs %>% mutate(gp=ifelse(count==0, 0, gp))
discs <- discs %>% mutate(gCIUpper=ifelse(count==0, 0, gCIUpper))

set.seed(42)
f<-ggplot(discs, aes(x=discs$Block, y=discs$gp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = discs$gCILower,ymax = discs$gCIUpper), color="black", width=0.5) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Gaming Discs/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.2), breaks=seq(0, 0.2, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
f
ggsave("MPAI_ByBlock_DiscsWBG.png", f, width=10, height=7.5, dpi=300)


#run function on data
cewCI <- adjustedWaldCI(discs$count,discs$gdCEW,0.05)
discs$cCIUpper <- cewCI$upperCL
discs$cCILower <- cewCI$lowerCL
discs$cp <- cewCI$pTilde

discs <- discs %>% mutate(cp=ifelse(count==0, 0, cp))
discs <- discs %>% mutate(cCIUpper=ifelse(count==0, 0, cCIUpper))

set.seed(42)
g<-ggplot(discs, aes(x=discs$Block, y=discs$cp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = discs$cCILower,ymax = discs$cCIUpper), color="black", width=0.25) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Gaming Discs/CEW Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.2), breaks=seq(0, 0.2, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
g
ggsave("MPAI_ByBlock_DiscsCEW.png", g, width=10, height=7.5, dpi=300)

## L. CEW Types ######################
mp1 <- filter(cewtype, Ware == 'Morne Patate Type 1')

# Join with WBG and CEW totals
mp1_a <- left_join(wbgCEW, mp1, by='Block')

# Replace NAs
discs <- discs %>% mutate(Category=ifelse(is.na(Category), 'GameDisc', Category))

discs <- discs %>% mutate(count=ifelse(is.na(count), 0, count))

# Calculate AIs
discs <- discs %>% mutate(gdCEW=count+CEW)
discs <- discs %>% mutate(cewAI=count/(count+CEW))

discs <- discs %>% mutate(gdWBG=count+WBG)
discs <- discs %>% mutate(wbgAI=count/(count+WBG))

#run function on form data
wbgCI <- adjustedWaldCI(discs$count,discs$gdWBG,0.05)
discs$gCIUpper <- wbgCI$upperCL
discs$gCILower <- wbgCI$lowerCL
discs$gp <- wbgCI$pTilde

discs <- discs %>% mutate(gp=ifelse(count==0, 0, gp))
discs <- discs %>% mutate(gCIUpper=ifelse(count==0, 0, gCIUpper))

set.seed(42)
f<-ggplot(discs, aes(x=discs$Block, y=discs$gp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = discs$gCILower,ymax = discs$gCIUpper), color="black", width=0.5) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Gaming Discs/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.2), breaks=seq(0, 0.2, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
f
ggsave("MPAI_ByBlock_DiscsWBG.png", f, width=10, height=7.5, dpi=300)


#run function on data
cewCI <- adjustedWaldCI(discs$count,discs$gdCEW,0.05)
discs$cCIUpper <- cewCI$upperCL
discs$cCILower <- cewCI$lowerCL
discs$cp <- cewCI$pTilde

discs <- discs %>% mutate(cp=ifelse(count==0, 0, cp))
discs <- discs %>% mutate(cCIUpper=ifelse(count==0, 0, cCIUpper))

set.seed(42)
g<-ggplot(discs, aes(x=discs$Block, y=discs$cp))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  geom_errorbar(aes(ymin = discs$cCILower,ymax = discs$cCIUpper), color="black", width=0.25) + 
  xlab("Block Area") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Gaming Discs/CEW Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,0.2), breaks=seq(0, 0.2, 0.05))+
  #  scale_x_continuous(limits=c(1670,1850)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold"))# +
#scale_fill_manual(name="State", values=c("blue", "orange"))
g
ggsave("MPAI_ByBlock_DiscsCEW.png", g, width=10, height=7.5, dpi=300)

