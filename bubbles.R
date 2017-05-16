Sys.setlocale(,"RU_Ru")
library(ggplot2)
library(readxl)
library(readr)
library(png)
library(grid)
library(extrafont)

#data <- read_excel("~/points/Копия-Копия-Персоны_Шарики_Игорь-15.xlsx", sheet = "Гройсман")
i <- max(c(sqrt(abs(parse_number(unname(unlist(data[2,2:8]))))),sqrt(abs(parse_number(unname(unlist(data[4,2:8])))))),na.rm=t)
p <- ggplot()+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=-2.3,ymax=0
  ),fill = '#ebebed')+ 
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=0.1,ymax=0.35
  ), fill = '#303d7d')+
  geom_linerange(aes(x= as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),ymax=0,ymin=-4.5),color="#babdbf")+
  geom_hline(yintercept = -4.5,color="#babdbf")+
  geom_segment(
    aes(
      y=-0.8,yend=-0.8,xend= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4
    )
  )+
  geom_segment(
    aes(
      y=-0.8,yend=-1,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")
    )
  )+
  geom_segment(
    aes(
      y=-1.8,yend=-1.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4
    )
  )+
  geom_segment(
    aes(
      y=-1.8,yend=-2,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")
    )
  )+
  geom_segment(
    aes(
      y=-2.8,yend=-2.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4
    )
  )+
  geom_segment(
    aes(
      y=-2.8,yend=-3,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")
    )
  )+
  geom_segment(
    aes(
      y=-3.8,yend=-3.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4
    )
  )+
  geom_segment(
    aes(
      y=-3.8,yend=-4,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),NA)),origin = "1970-01-01")
    )
  )+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=-5.2,ymax=-5
  ),fill=NA,color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=-5.4,ymax=-5.2
  ),fill="#d8d9da",color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=-5.6,ymax=-5.4
  ),fill=NA,color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-0.5,ymin=-5.8,ymax=-5.6
  ),fill="#808083",color="#babdbf")+
  geom_segment(aes(
    y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.5,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.5
  ),color="#babdbf")+
  geom_segment(aes(
    y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0
  ),color="#babdbf")+
  geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.25,label=format(parse_number(unname(unlist(data[18,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.25,label=format(parse_number(unname(unlist(data[19,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.25,label=format(parse_number(unname(unlist(data[20,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white")+
  geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.75,label=format(parse_number(unname(unlist(data[18,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.75,label=format(parse_number(unname(unlist(data[19,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+0.75,label=format(parse_number(unname(unlist(data[20,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white")+
  geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+0.25,label="Нейтрально"),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+0.75,label="Негативно"),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.3,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.35,label="TV"),family="PT Sans", fontface = "bold",color="#303d7d",hjust=0)+
  geom_text(aes(y=-5.5,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.35,label="Internet"),family="PT Sans", fontface = "bold",color="#303d7d",hjust=0)+
  geom_text(aes(y=-5.7,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.35,label="Разом:"),family="PT Sans", fontface = "bold",color="white",hjust=0)+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.6,ymin=-5.2,ymax=-5
  ),fill=NA,color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.6,ymin=-5.4,ymax=-5.2
  ),fill="#d8d9da",color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.6,ymin=-5.6,ymax=-5.4
  ),fill=NA,color="#babdbf")+
  geom_rect(aes(
    xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.6,ymin=-5.8,ymax=-5.6
  ),fill="#808083",color="#babdbf")+
  geom_segment(aes(
    y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.1,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.1
  ),color="#babdbf")+
  geom_segment(aes(
    y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+1.6,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+1.6
  ),color="#babdbf")+
  geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.35,label=format(parse_number(unname(unlist(data[18,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.35,label=format(parse_number(unname(unlist(data[19,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+2.35,label=format(parse_number(unname(unlist(data[20,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white")+
  geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+1.85,label=format(parse_number(unname(unlist(data[18,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+1.85,label=format(parse_number(unname(unlist(data[19,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"))+1.85,label=format(parse_number(unname(unlist(data[20,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white")+
  geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+1.85,label="Нейтрально"),family="PT Sans", fontface = "bold",color="#303d7d")+
  geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+2.35,label="Негативно"),family="PT Sans", fontface = "bold",color="#a31e22")+
  geom_text(aes(y=-5.3,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+1.25,label="TV"),family="PT Sans", fontface = "bold",hjust=0,color="#303d7d")+
  geom_text(aes(y=-5.5,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+1.25,label="Internet"),family="PT Sans", fontface = "bold",hjust=0,color="#303d7d")+
  geom_text(aes(y=-5.7,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+1.25,label="Разом:"),family="PT Sans", fontface = "bold",hjust=0,color="white")+
  geom_text(aes(y=-4.95,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.5,label="Контакти з аудиторією"),size=5, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="#babdbf")+
  geom_text(aes(y=-4.95,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]+1.1,label="Кількість матеріалів"),size=5, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="#babdbf")+
  geom_text(aes(y=0.14,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.48,label="Характеристика інформаційного поля за"),size=9, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="white")+
  geom_text(aes(y=-5.9,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1]-0.5,label="Тижневий огляд інформаційного поля АПУ"),size=4, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="#babdbf")+
  geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-1),size=sqrt(abs(parse_number(unname(unlist(data[2,2:8])))))/i*20,color=
                      ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"#303d7d","#a31e22"))+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-1.2,label=format(abs(parse_number(unname(unlist(data[2,2:8])))),  big.mark=" ")),color=
              ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"#303d7d","#a31e22"),family="PT Sans")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")-0.4,-0.75,label=unlist(unname(sapply(data[3,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),25), collapse="\n"))))),size=3.8, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color=
              ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"black","#a31e22"),size=4)+
  geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-2),size=sqrt(parse_number(unname(unlist(data[4,2:8]))))/i*20,color=
  ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"#303d7d","#a31e22"))+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-2.2,label=format(parse_number(unname(unlist(data[4,2:8]))),  big.mark=" ")),color=
              ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"#303d7d","#a31e22"),family="PT Sans")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")-0.4,-1.75,label=unlist(unname(sapply(data[5,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),25), collapse="\n"))))),size=3.8, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color=
              ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"black","#a31e22"),size=4)+
  geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-3),size=sqrt(parse_number(unname(unlist(data[10,2:8]))))/i*20,color="#303d7d")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-3.2,label=format(parse_number(unname(unlist(data[10,2:8]))),  big.mark=" ")),color="#303d7d",family="PT Sans")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")-0.4,-2.75,label=unlist(unname(sapply(data[11,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),25), collapse="\n"))))),color="black",size=3.8, lineheight=0.7,hjust=0,vjust=0,family="PT Sans")+
  geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-4),size=sqrt((-parse_number(unname(unlist(data[12,2:8])))))/i*20,color="#a31e22")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-4.2,label=format(-parse_number(unname(unlist(data[12,2:8]))),  big.mark=" ")),color="#a31e22",family="PT Sans")+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")-0.4,-3.75,label=unlist(unname(sapply(data[13,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),25), collapse="\n"))))),color="#a31e22",size=3.8, lineheight=0.7,hjust=0,vjust=0,family="PT Sans")+
  geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-4.5,size=parse_number(unname(unlist(data[10,2:8])))),shape=22,color="#babdbf",fill=c("#babdbf","#babdbf","#babdbf","#babdbf","#babdbf","white","white"))+
  geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23"),-4.6,label=substr(as.character(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")),9,10)),color="#a31e22",family="PT Sans", fontface = "bold")+
  annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.5, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.35, ymin=-2.2, ymax=-2)+
  annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.2, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.1, ymin=-5.4, ymax=-5.25)+
  annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.5, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.4, ymin=-5.4, ymax=-5.25)+
  annotation_custom(rasterGrob(readPNG("2.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.2, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.1, ymin=-5.6, ymax=-5.45)+
  annotation_custom(rasterGrob(readPNG("2.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.5, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.4, ymin=-5.6, ymax=-5.45)+
  annotation_custom(rasterGrob(readPNG("3.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.2, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])+1.1, ymin=-5.75, ymax=-5.65)+
  annotation_custom(rasterGrob(readPNG("3.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.5, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-23")[1])-0.4, ymin=-5.75, ymax=-5.65)+
  theme_void(base_family="PT Sans")+
#scale_x_date(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  theme(
    legend.position = "none",
    text = element_blank(),
    line = element_blank(),
    title = element_blank())

gt <- ggplot_gtable(ggplot_build(p))
ge <- subset(gt$layout, name == "panel")

grid.draw(gt[ge$t:ge$b, ge$l:ge$r])

img <- readPNG("1.png")
g <- rasterGrob(readPNG("1.png"), interpolate=TRUE)


ggplot() +
  geom_point(aes(mtcars$mpg[1],mtcars$cyl[1]))+
  annotation_custom(g, xmin=20, xmax=21, ymin=5, ymax=6)
  
