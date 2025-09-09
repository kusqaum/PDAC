# plot for discrimination
discrimKMPlot <- function(fit, data, time, pal="none", leg.x="none",leg.y="none", leg.title="none",
                        xlim ="auto",leg.labs="none", hl=1,vl="none",...){
  time <- as.numeric(time)
  if(leg.x == "none") leg.x<-0.7
  if(leg.y == "none") leg.y<-0.7
  if(leg.title == "none") leg.title<-element_blank()
  if(xlim[1]== "auto") xlim <-c(0,max(time,na.rm = TRUE))
  if(vl=="none") vl <- xlim[2]
  if(is.character(leg.labs) & length(leg.labs)!=length(fit$n)) leg.labs <- as.character(seq(1,length(fit$n)))
  ggsurvplot(fit, data=data,
             palette = pal,
             legend = c(leg.x,leg.y),
             legend.title = leg.title,
             xlim = xlim,
             legend.labs = leg.labs,
             xlab="Time (months)"
             )$plot+
    geom_hline(yintercept = seq(0,hl, by = 0.1), lty = 2, colour="grey") +
    geom_vline(xintercept = seq(0,vl, by = 10), lty = 2, colour="grey") +
    theme_mecPortal()
    
    
}

theme_mecPortal <- function(){
    theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', colour=NA),
        legend.background = element_rect(fill='transparent', colour = NA),
        #legend.key = element_rect(fill = "transparent",colour = NA),
        legend.box.background = element_rect(fill='transparent'),
        axis.title.x = element_text(face="italic", colour="white"),
        axis.title.y = element_text(face = "italic", colour="white"),
        legend.text = element_text(colour="white"),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))
}
"#EF9A9A"
greens <-c("#E0F2F1","#80CBC4","#00BFA5","#00796b")
pinks<- c("#FCE4EC","#F8BBD0","#FF80AB","#C51162")
pinks<- c("#FCE4EC","#F8BBD0","#F06292","#C51162")
c("#FCE4EC","#F8BBD0","#E57373","#FF80AB", "#F06292","#AD1457")
pinksCb<- c("#feebe2","#fbb4b9","#f768a1","#ae017e")