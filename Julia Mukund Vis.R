mydata2 <- read.csv(
  "C:/Users/julia/OneDrive/Desktop/shiny-seaside/pre_event_cleaned_dummy.csv")
glimpse(mydata2)

print(mydata2$previous_attendance)
p<-ggplot(data=mydata2, aes(previous_attendance)) +
  geom_bar(width=0.5, color='blue', fill='blue')
p
p + labs(title='Previously Attended Seaside Event', x='Response', y='Count')

print(mydata2$find_out_event)
p2<-ggplot(data=mydata2, aes(find_out_event)) +
  geom_bar(width=0.5, color='blue', fill='blue')+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p2 + labs(title='How did you find out about Seaside event?', x='Response', y='Count')

p3<-ggplot(data=mydata2, aes(pronoun)) +
  geom_bar(width=0.5, color='blue', fill='blue')+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p3 + labs(title='Pronouns', x='Response', y='Count')