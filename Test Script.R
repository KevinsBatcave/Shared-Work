library(reshape) 
library(plyr) 
library(gtools) 
library(stringr)
library(sqldf)
library(reshape2)
library(dplyr)
library(foreach)
library(doParallel)
library(ggplot2)
library(scales)

##The data
TestVector <- c('SubSegment','AOrd','Segment','QOrd','Question','Answer','Respondents','SegTotals','Letters','Significance')
TestVector2 <- c('Group 1','1','Grouping',232,'Test Question','Very well',129,231,'A','')
TestVector3 <- c('Group 1','2','Grouping',232,'Test Question','Somewhat well',77,231,'A','')
TestVector4 <- c('Group 1','3','Grouping',232,'Test Question','Neither well nor poor',14,231,'A','')
TestVector5 <- c('Group 1','4','Grouping',232,'Test Question','Somewhat poorly',9,231,'A','c')
TestVector6 <- c('Group 1','5','Grouping',232,'Test Question','Very poorly',2,231,'A','')
TestVector7 <- c('Group 2','1','Grouping',232,'Test Question','Very well',124,236,'B','')
TestVector8 <- c('Group 2','2','Grouping',232,'Test Question','Somewhat well',92,236,'B','')
TestVector9 <- c('Group 2','3','Grouping',232,'Test Question','Neither well nor poor',11,236,'B','')
TestVector10 <- c('Group 2','4','Grouping',232,'Test Question','Somewhat poorly',8,236,'B','')
TestVector11 <- c('Group 2','5','Grouping',232,'Test Question','Very poorly',1,236,'B','')
TestVector12 <- c('Group 3','1','Grouping',232,'Test Question','Very well',107,217,'C','')
TestVector13 <- c('Group 3','2','Grouping',232,'Test Question','Somewhat well',93,217,'C','a')
TestVector14 <- c('Group 3','3','Grouping',232,'Test Question','Neither well nor poor',13,217,'C','')
TestVector15 <- c('Group 3','4','Grouping',232,'Test Question','Somewhat poorly',3,217,'C','')
TestVector16 <- c('Group 3','5','Grouping',232,'Test Question','Very poorly',1,217,'C','')
TestVector17 <- c('Group 4','1','Grouping',232,'Test Question','Very well',109,211,'D','')
TestVector18 <- c('Group 4','2','Grouping',232,'Test Question','Somewhat well',84,211,'D','')
TestVector19 <- c('Group 4','3','Grouping',232,'Test Question','Neither well nor poor',11,211,'D','')
TestVector20 <- c('Group 4','4','Grouping',232,'Test Question','Somewhat poorly',6,211,'D','')
TestVector21 <- c('Group 4','5','Grouping',232,'Test Question','Very poorly',1,211,'D','')

TestVector1<- rbind(TestVector2,TestVector3,TestVector4,TestVector5,TestVector6,TestVector7,TestVector8,TestVector9,TestVector10,TestVector11,TestVector12,TestVector13,TestVector14,TestVector15,TestVector16,TestVector17,TestVector18,TestVector19,TestVector20,TestVector21)

colnames(TestVector1) <- TestVector

TotalsPartsChart1 <- as.data.frame(TestVector1)

TotalsPartsChart1$Respondents <- as.numeric(as.character(TotalsPartsChart1$Respondents))
TotalsPartsChart1$SegTotals <- as.numeric(as.character(TotalsPartsChart1$SegTotals))

TotalsPartsChart1$Answer <- as.factor(as.character(TotalsPartsChart1$Answer))
TotalsPartsChart1$Answer<- factor(TotalsPartsChart1$Answer, levels=c('Very well','Somewhat well','Neither well nor poor','Somewhat poorly','Very poorly'))

##The chart making part
print(ggplot(TotalsPartsChart1, aes(fill=SubSegment, y=Respondents/SegTotals, x=Answer))
      ##Text labels which give an extra line if statistical significance is found; the rest is mainly about having it look decent
      +  geom_text(aes(label = ifelse(Significance != '',paste(" ",percent(Respondents/SegTotals),"\n",Significance),paste(" ",percent(Respondents/SegTotals)))
                   , y = Respondents/SegTotals +.017)
                   , position = position_dodge(0.9)
                   , size = 2.5
                   , fontface = "bold"
                   , vjust = .25) 
      ##Secondary bar to highlight significant bars; I'd like to get rid of this in lieu of having the significant labels be red instead
      + scale_color_manual(values = c("red"))
      + geom_bar(position="dodge", stat="identity")
      ##Normally these actually refer to another dataframe for the Title and Legend title as it's part of a loop, but I made them set for the purposes of this
      + labs(title=str_wrap("Test Question"),fill="Grouping") + ylab("Percentage of Respondents")
      + scale_y_continuous(labels=percent)
      + scale_x_discrete(labels = function(x) str_wrap(TotalsPartsChart1$Answer, width = 10))
      + theme(plot.title = element_text(hjust = 0.5))
      ##The rest of the secondary bar; if there's significance it highlights the whole bar. If I can't get the text to be red I'd settle for removing the red outline at the bottom of everything that isn't significant
      + geom_bar(position="dodge"
                 , stat="identity"
                 , aes(fill= TotalsPartsChart1$SubSegment, y=ifelse(TotalsPartsChart1$Significance != "",TotalsPartsChart1$Respondents/TotalsPartsChart1$SegTotals,.00)
                       , color = "red"
                       , x=Answer)
                 , show.legend = F) 
) 

