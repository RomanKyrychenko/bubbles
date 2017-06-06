Sys.setlocale(,"UK_ua")
library(shiny)
library(ggplot2)
library(readxl)
library(readr)
library(png)
library(grid)
library(extrafont)
library(stringi)

tele <- rasterGrob(readPNG("1.png"), interpolate=TRUE)
net <- rasterGrob(readPNG("2.png"), interpolate=TRUE)
zag <- rasterGrob(readPNG("3.png"), interpolate=TRUE)

ui <- shinyUI(fluidPage(
  titlePanel("Corestone GR-LU"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('downloadPlot',"Завантажити в pdf!"),
      downloadButton('download',"Завантажити в png!")
    ),
    mainPanel("Візуалізація",plotOutput('plot', width = "1600px", height = "900px"))
  )
)
)

server <- shinyServer(function(input, output){
  df <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), sheet = 1,col_names = F)
    data[c(4,6,8,10,12,14,16),1] <- zoo::na.locf(data[c(4,6,8,10,12,14,16),1])
    data[c(4,6,8,10,12,14,16)+1,6]<-ifelse(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))==0,NA,
                                           unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))
    data[c(4,6,8,10,12,14,16)+1,6]<-ifelse(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))==0,NA,
                                           unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))
    data[c(4,6,8,10,12,14,16),3]<-ifelse(unname(unlist(data[c(4,6,8,10,12,14,16),3]))==0,NA,
                                         unname(unlist(data[c(4,6,8,10,12,14,16),3])))
    data[c(4,6,8,10,12,14,16)+1,3]<-ifelse(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))==0,NA,
                                           unname(unlist(data[c(4,6,8,10,12,14,16)+1,3])))
    bubble <- function(data){
      img <- readPNG("1.png")
      i <- max(sqrt(abs(parse_number(unname(unlist(c(data[4:17,3],data[4:17,6])))))),na.rm=T)
      #shrift <- 7.5+(5.7-max(c(nchar(data[4:17,2]),nchar(data[4:17,4])),na.rm = T)/26)/2-3
      #otst <- 10+33-shrift*6
      otst <- 25
      shrift <- 4.5
      p <- ggplot()+
        geom_segment(aes(
          y = -4.5,yend=-4.5,xend=max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                              origin = "1899-12-30"))+0.6, 
          x = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.75),
          color="#babdbf"
        )+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                             origin = "1899-12-30"))+0.6, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.75,
          ymin=-2.3,ymax=-0.2
        ),fill = '#ebebed')+ 
        geom_linerange(aes(x= as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                           ymax=-0.2,ymin=-4.5),color="#babdbf")+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-0.8,yend=-0.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                                                         as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                                 origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,
            x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                                      as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                                      NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-0.8,yend=-1,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                                                    as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                            origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,
            xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6]))))),
                                        as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-1.8,yend=-1.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                                                         as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                                 origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,
            x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                                      as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                                      NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-1.8,yend=-2,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                                                    as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                            origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,
            xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))))),
                                        as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-2.8,yend=-2.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                                                         as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                                 origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,
            x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                                      as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                                      NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-2.8,yend=-3,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                                                    as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                            origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,
            xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))),
                                        as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                                        NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-3.8,yend=-3.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                                                         as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                                 origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,
            x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                                      as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),NA)),
                       origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                             as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                             NA)))!=7)geom_segment(
          aes(
            y=-3.8,yend=-4,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                                                    as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                            origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,
            xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                                        as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),-1),
                   size=sqrt(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6])))))/i*25,color=
                     ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6])))>0,"#303d7d","#a31e22"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),-1.2,
                      label=format(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6])))),  
                                   big.mark=" ")),color=
                    ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6])))>0,"#303d7d","#a31e22"),
                  family="PT Sans",size=5)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30")-0.4,
                      -0.75,label=unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16),5])),
                                                       function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                      fixed="TRUE"),otst), 
                                                                         collapse="\n"))))),
                  size=ifelse(nchar(unlist(unname(data[c(4,6,8,10,12,14,16),2])))<80,4.5,shrift), 
                  lineheight=0.9,hjust=0,vjust=0,family="PT Sans",color=
                    ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),6])))>0,"black","#a31e22"))+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),-2),
                   size=sqrt(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))))/i*25,color=
                     ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))>0,"#303d7d","#a31e22"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),-2.2,
                      label=format(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))),  big.mark=" ")),color=
                    ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))>0,"#303d7d","#a31e22"),
                  family="PT Sans",size=5)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30")-0.4,
                      -1.75,label=unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16)+1,5])),
                                                       function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                      fixed="TRUE"),otst), 
                                                                         collapse="\n"))))),
                  size=ifelse(nchar(unlist(unname(data[c(4,6,8,10,12,14,16)+1,5])))<80,4.5,shrift), 
                  lineheight=0.9,hjust=0,vjust=0,family="PT Sans",color=
                    ifelse(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6])))>0,"black","#a31e22"))+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                       -3),size=sqrt(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))))/i*25,color="#303d7d")+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                      -3.2,label=ifelse(grepl("NA",format(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3])))))),
                                        NA,format(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3])))),  
                                                  big.mark=" "))),color="#303d7d",family="PT Sans",size=5)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30")-0.4,
                      -2.75,label=ifelse(unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16),2])),
                                                              function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                             fixed="TRUE"),20), 
                                                                                collapse="\n"))))=="NA",NA,
                                         unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16),2])),
                                                              function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                             fixed="TRUE"),otst), 
                                                                                collapse="\n")))))),color="black",
                  size=ifelse(nchar(unlist(unname(data[c(4,6,8,10,12,14,16),1])))<80,4.5,shrift), 
                  lineheight=0.9,hjust=0,vjust=0,family="PT Sans")+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                       -4),size=sqrt((-parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3])))))/i*25,color="#a31e22")+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                      -4.2,label=ifelse(grepl("NA",format(-parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3]))))),
                                        NA,format(abs(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3])))),  
                                                  big.mark=" "))),color="#a31e22",family="PT Sans",size=5)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30")-0.4,
                      -3.75,label=ifelse(unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16),2])),
                                                              function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                             fixed="TRUE"),20), 
                                                                                collapse="\n"))))=="NA",NA,
                                         unlist(unname(sapply(unlist(unname(data[c(4,6,8,10,12,14,16)+1,2])),
                                                              function(x) paste(strwrap(gsub("((","",unname(unlist(x)), 
                                                                                             fixed="TRUE"),otst), 
                                                                                collapse="\n")))))),color="#a31e22",
                  size=ifelse(nchar(unlist(unname(data[c(4,6,8,10,12,14,16),2])))<80,4.5,shrift), 
                  lineheight=0.9,hjust=0,vjust=0,family="PT Sans")+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                       -4.5,size=colSums(rbind(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))),
                                               parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,6]))),
                                               parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),3]))),
                                               parse_number(unname(unlist(data[c(4,6,8,10,12,14,16)+1,3])))),na.rm = T)),
                   shape=22,color="#babdbf",fill=c("#babdbf","#babdbf","#babdbf","#babdbf","#babdbf","white","white"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"),
                      -4.6,label=substr(as.character(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                             origin = "1899-12-30")),9,10)),color="#a31e22",
                  family="PT Sans", fontface = "bold",size=5)+
        annotation_custom(tele, 
                          xmin=as.numeric(as.Date(parse_number(unname(unlist(data[4,1]))),
                                                  origin = "1899-12-30")[1])-0.7, 
                          xmax=as.numeric(as.Date(parse_number(unname(unlist(data[4,1]))),
                                                  origin = "1899-12-30")[1])-0.55, ymin=-2.2, ymax=-2)+
        annotation_custom(net, 
                          xmin=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.7, 
                          xmax=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.55, ymin=-2.6, ymax=-2.4)+
        theme_void(base_family="PT Sans")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-5.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-5.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-5.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        geom_segment(aes(
          y=-5,yend=-5.4,xend=min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                          origin = "1899-12-30"))-0.2,
          x=min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.2
        ),color="#babdbf")+
        #table 2
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-4.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-4.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-4.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        #table 3
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-3.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+1.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-3.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+1.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-3.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+1.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        #table 4
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-2.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+2.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-2.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+2.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-2.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+2.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        #table 5
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-1.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+3.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-1.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+3.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-1.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+3.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        #table 6
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+4.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+4.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))-0.7, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+4.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        #table 7
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.3, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+5.7,
          ymin=-5.2,ymax=-5
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.3, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+5.7,
          ymin=-5.4,ymax=-5.2
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+0.3, 
          xmin = min(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),origin = "1899-12-30"))+5.7,
          ymin=-5.6,ymax=-5.4
        ),fill="#808083",color="#babdbf")+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[3:9,8]))),origin = "1899-12-30"),-5.1,
                      label=format(parse_number(unname(unlist(data[3:9,9]))),big.mark=" ")),
                  family="PT Sans", fontface = "bold",color="#303d7d",size=5
        )+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[3:9,8]))),origin = "1899-12-30"),-5.3,
                      label=format(parse_number(unname(unlist(data[3:9,10]))),big.mark=" ")),family="PT Sans", 
                  fontface = "bold",color="#303d7d",size=5
        )+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[3:9,8]))),origin = "1899-12-30"),-5.5,
                      label=format(parse_number(unname(unlist(data[3:9,11]))),big.mark=" ")),family="PT Sans", 
                  fontface = "bold",color="#303d7d",size=5
        )+
        annotation_custom(tele, 
                          xmin=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.65, 
                          xmax=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.55, ymin=-5.05, ymax=-5.15)+
        annotation_custom(net, 
                          xmin=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.65, 
                          xmax=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.55, ymin=-5.25, ymax=-5.35)+
        annotation_custom(zag, 
                          xmin=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.65, 
                          xmax=as.numeric(as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                                  origin = "1899-12-30")[1])-0.55, ymin=-5.45, ymax=-5.55)+
       geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                    origin = "1899-12-30")[1]-0.52,label="TV"),family="PT Sans", 
                 fontface = "bold",color="#303d7d",hjust=0,size=5)+
       geom_text(aes(y=-5.3,as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                    origin = "1899-12-30")[1]-0.52,label="Internet"),family="PT Sans", 
                 fontface = "bold",color="#303d7d",hjust=0,size=5)+
       geom_text(aes(y=-5.5,as.Date(parse_number(unname(unlist(data[c(4,6,8,10,12,14,16),1]))),
                                    origin = "1899-12-30")[1]-0.52,label="Разом:"),family="PT Sans", 
                 fontface = "bold",color="white",hjust=0,size=5)+
        scale_x_date(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
        theme(
          legend.position = "none",
          text = element_blank(),
          line = element_blank(),
          title = element_blank()
        )
      gt <- ggplot_gtable(ggplot_build(p))
      ge <- subset(gt$layout, name == "panel")
      
      grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
      p
    }
    bubble(data)
  })
  output$plot <- renderPlot({
    tryCatch(df())
  })
  output$downloadPlot <-  downloadHandler(
    filename = function(){paste0("grolu-",Sys.Date(),".pdf") },
    content = function(file) {
      cairo_pdf(file, width=16*1.4, height=9*1.4)
      print(df())
      dev.off()
    }
  )
  
  output$download<-  downloadHandler(
    filename = function(){paste0("grolu-",Sys.Date(),".png") },
    content = function(file) {
      png(file, width=1600, height=900)
      print(df())
      dev.off()
    }
  )
})

shinyApp(ui,server)
