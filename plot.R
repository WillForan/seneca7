library(dplyr); library(lubridate); library(ggplot2)

#d <-
#  system('xclip -o',intern=T) %>% 
#  textConnection %>% 
#  read.table(sep="\t",quote="",comment.char="")%>%

d.raw <- 
  read.table('legs.txt',sep="\t",quote="",comment.char="")%>%
  `names<-`(c('bib','team',paste0('leg',sprintf('%02d',1:21)),'avg')) 

abrvteam <- function(teamname)  toupper(gsub('\\W?([A-Za-z])[^ ]*','\\1',teamname))

d.rank <-
  d.raw %>%
  mutate_at(vars(matches('leg|avg')),funs(ms(.) %>% seconds)) %>% 
  arrange(avg) %>% 
  mutate(rank=rank(avg)) 

ourbib <- 58
ourrank <- d.rank$rank[d.rank$bib==ourbib] #27 (accounting for ties)

otherteams <-c('Team GTB','Slay then Rose','Catch Me Stridin Dirty','ADK Honey Badgers #2')

d.long <-  
 d.rank %>%
 # only teams within 2 of us
 filter( abs(rank-27) < 2 | team %in% otherteams ) %>% 
 gather(leg,dur,-bib,-team,-avg,-rank) %>% 
 # asign runner number
 mutate(leg=gsub('leg','',leg)%>%as.numeric ,
        runno=(leg-1)%%7 +1,
        runnerleg=floor((leg-1)/7+1)) %>%
 # total
 group_by(bib) %>% 
 mutate(totaldur=cumsum(dur)) %>%
 # normalize by mean leg duration
 group_by(leg) %>% 
 mutate(min.from.legavg.time= (totaldur-mean(totaldur,na.rm=T))/60 )



#p.all<-ggplot(d.long) + 
#  aes(x=leg,y=min.from.legavg.time,color=team,group=bib,label=rank) + 
#  geom_line() +
#  geom_line(data=d.long%>%filter(bib==ourbib),color='red',size=1.1,linetype=2) +
#  theme_bw() 
#print(p.all)

p<-ggplot(d.long) + 
  aes(x=leg,y=min.from.legavg.time,color=team,group=bib,label=abrvteam(team)) + 
  geom_line() +
  geom_point() + 
  geom_line(data=d.long%>%filter(bib==ourbib),color='red',size=1.1,linetype=2) +
  #geom_text(data=d.long%>%filter(runnerleg==3),color='black',hjust=-.1) +
  #scale_x_continuous(limits=c(1,23)) +
  theme_bw() + facet_wrap(~runno) 
print(p)
ggsave('avglegXrunner.png')
