##Turn the NAs into blanks as that's how the script normally gives them out
TotalsPartsChart[is.na(TotalsPartsChart==TRUE)] <- ''

##The chart making part
print(ggplot(TotalsPartsChart, aes(fill=SubSegment, y=Respondents/SegTotals, x=Answer))
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
      + scale_x_discrete(labels = function(x) str_wrap(TotalsPartsChart$Answer, width = 10))
      + theme(plot.title = element_text(hjust = 0.5))
      ##The rest of the secondary bar; if there's significance it highlights the whole bar. If I can't get the text to be red I'd settle for removing the red outline at the bottom of everything that isn't significant
      + geom_bar(position="dodge"
                 , stat="identity"
                 , aes(fill= TotalsPartsChart$SubSegment, y=ifelse(TotalsPartsChart$Significance != "",TotalsPartsChart$Respondents/TotalsPartsChart$SegTotals,.00)
                       , color = "red"
                       , x=Answer)
                 , show.legend = F) 
) 
