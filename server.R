shinyServer(function(input, output){
  df <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), sheet = input$typ,col_names = T)
    d <-read_excel(paste(inFile$datapath, ".xlsx", sep=""),sheet = "ТВ",col_names = T)
    data[2,2:8]<-ifelse(unname(unlist(data[2,2:8]))==0,NA,unname(unlist(data[2,2:8])))
    data[4,2:8]<-ifelse(unname(unlist(data[4,2:8]))==0,NA,unname(unlist(data[4,2:8])))
    data[10,2:8]<-ifelse(unname(unlist(data[10,2:8]))==0,NA,unname(unlist(data[10,2:8])))
    data[12,2:8]<-ifelse(unname(unlist(data[12,2:8]))==0,NA,unname(unlist(data[12,2:8])))
    bubble <- function(data){
      person_name <- if(input$typ=="Президент"){
        "ПЕТРО ПОРОШЕНКО"
      } else if (input$typ=="Гройсман"){
        "ВОЛОДМИРИ ГРОЙСМАН"
      } else if (input$typ=="Райнин"){
        "ІГОР РАЙНІН"
      } else if (input$typ=="Луценко"){
        "ЮРІЙ ЛУЦЕНКО"
      } else if (input$typ=="Геращенко"){
        "ІРИНА ГЕРАЩЕНКО"
      } else if (input$typ=="Шимкив"){
        "ДМИТРО ШИМКІВ"
      } else if (input$typ=="Ковальчук"){
        "ВІТАЛІЙ КОВАЛЬЧУК"
      } else if (input$typ=="Зубко"){
        "ГЕННАДІЙ ЗУБКО"
      } else if (input$typ=="Гонтарева"){
        "ВАЛЕРІЯ ГОНТАРЕВА"
      } else if (input$typ=="Муженко"){
        "ВІКТОР МУЖЕНКО"
      } else if (input$typ=="Ложкин"){
        "БОРИС ЛОЖКІН"
      } else if (input$typ=="Елисеев"){
        "КОСТЯНТИН ЄЛІСЄЄВ"
      } else if (input$typ=="Хромаев"){
        "ТИМУР ХРОМАЄВ"
      } else if (input$typ=="Терентьев"){
        "ЮРІЙ ТЕРЕНТЬЄВ"
      } else if (input$typ=="Филатов"){
        "ОЛЕКСІЙ ФІЛАТОВ"
      } else if (input$typ=="Билоус"){
        "ІГОР БІЛОУС"
      } else{
        "МАРИНА ПОРОШЕНКО"
      }
      img <- readPNG("1.png")
      g <- rasterGrob(readPNG("1.png"), interpolate=TRUE)
      i <- max(sqrt(d$Итог),na.rm=T)
      shrift <- 5.5+(5.7-max(c(nchar(data[3,2:8]),nchar(data[5,2:8]),nchar(data[11,2:8]),nchar(data[13,2:8])),na.rm = T)/26)/2
      shrift <-ifelse(input$typ!="Президент",shrift,shrift-0.5)
      otst <- 31+33-shrift*6
      otst <- ifelse(input$typ!="Президент",otst,otst-6)
      p <- ggplot()+
        geom_segment(aes(
          y = -4.5,yend=-4.5,xend=max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.6, x = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.75),color="#babdbf"
        )+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.6, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.75,ymin=-2.3,ymax=-0.2
        ),fill = '#ebebed')+ 
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.6, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.75,ymin=-0.1,ymax=0.15
        ), fill = '#303d7d')+
        geom_linerange(aes(x= as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),ymax=-0.2,ymin=-4.5),color="#babdbf")+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-0.8,yend=-0.8,xend= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-0.8,yend=-1,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[2,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
            aes(
              y=-1.8,yend=-1.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4
            )
          )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-1.8,yend=-2,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[4,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-2.8,yend=-2.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-2.8,yend=-3,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[10,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-3.8,yend=-3.8,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")+0.4,x= as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4
          )
        )}+
        {if(sum(is.na(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)))!=7)geom_segment(
          aes(
            y=-3.8,yend=-4,x=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")-0.4,xend=as.Date(na.omit(ifelse(!is.na(abs(parse_number(unname(unlist(data[12,2:8]))))),as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),NA)),origin = "1970-01-01")
          )
        )}+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.5,ymin=-5.2,ymax=-5
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.5,ymin=-5.4,ymax=-5.2
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.5,ymin=-5.6,ymax=-5.4
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-5, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-0.5,ymin=-5.8,ymax=-5.6
        ),fill="#808083",color="#babdbf")+
        geom_segment(aes(
          y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.5,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.5
        ),color="#babdbf")+
        geom_segment(aes(
          y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0
        ),color="#babdbf")+
        geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.25,label=format(parse_number(unname(unlist(data[18,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.25,label=format(parse_number(unname(unlist(data[19,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.25,label=format(parse_number(unname(unlist(data[20,2]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white",size=6)+
        geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.75,label=format(parse_number(unname(unlist(data[18,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.75,label=format(parse_number(unname(unlist(data[19,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+0.75,label=format(parse_number(unname(unlist(data[20,3]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white",size=6)+
        geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+0.25,label="Нейтрально"),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+0.75,label="Негативно"),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        geom_text(aes(y=-5.3,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.32,label="TV"),family="PT Sans", fontface = "bold",color="#303d7d",hjust=0,size=6)+
        geom_text(aes(y=-5.5,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.32,label="Internet"),family="PT Sans", fontface = "bold",color="#303d7d",hjust=0,size=6)+
        geom_text(aes(y=-5.7,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.32,label="Разом:"),family="PT Sans", fontface = "bold",color="white",hjust=0,size=6)+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.6,2.1),ymin=-5.2,ymax=-5
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.6,2.1),ymin=-5.4,ymax=-5.2
        ),fill="#d8d9da",color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.6,2.1),ymin=-5.6,ymax=-5.4
        ),fill=NA,color="#babdbf")+
        geom_rect(aes(
          xmax = max(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))-4.9, xmin = min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.6,2.1),ymin=-5.8,ymax=-5.6
        ),fill="#808083",color="#babdbf")+
        geom_segment(aes(
          y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.1,1.6),x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.1,1.6)
        ),color="#babdbf")+
        {if(input$typ!="Президент")geom_segment(aes(
          y=-5,yend=-5.8,xend=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+1.6,x=min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+1.6
        ),color="#babdbf")}+
        geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.35,1.85),label=format(parse_number(unname(unlist(data[18,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.35,1.85),label=format(parse_number(unname(unlist(data[19,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",2.35,1.85),label=format(parse_number(unname(unlist(data[20,5]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white",size=6)+
        geom_text(aes(y=-5.3,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",1.85,1.35),label=format(parse_number(unname(unlist(data[18,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.5,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",1.85,1.35),label=format(parse_number(unname(unlist(data[19,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.7,min(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"))+ifelse(input$typ!="Президент",1.85,1.35),label=format(parse_number(unname(unlist(data[20,4]))),big.mark=" ")),family="PT Sans", fontface = "bold",color="white",size=6)+
        geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+ifelse(input$typ!="Президент",1.85,1.35),label="Нейтрально"),family="PT Sans", fontface = "bold",color="#303d7d",size=6)+
        geom_text(aes(y=-5.1,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+ifelse(input$typ!="Президент",2.35,1.85),label="Негативно"),family="PT Sans", fontface = "bold",color="#a31e22",size=6)+
        {if(input$typ!="Президент")geom_text(aes(y=-5.3,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+1.28,label="TV"),family="PT Sans", fontface = "bold",hjust=0,color="#303d7d",size=6)}+
        {if(input$typ!="Президент")geom_text(aes(y=-5.5,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+1.28,label="Internet"),family="PT Sans", fontface = "bold",hjust=0,color="#303d7d",size=6)}+
        {if(input$typ!="Президент")geom_text(aes(y=-5.7,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+1.28,label="Разом:"),family="PT Sans", fontface = "bold",hjust=0,color="white",size=6)}+
        geom_text(aes(y=-4.95,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.5,label="Контакти з аудиторією"),size=7, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="#808083")+
        geom_text(aes(y=-4.95,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]+1.1,label="Кількість матеріалів"),size=7, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="#808083")+
        geom_text(aes(y=-0.04,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.7,label=paste0(person_name,". Характеристика інформаційного поля за ",input$period)),size=13, lineheight=0.7,hjust=0,vjust=0,family="PT Sans",color="white")+
        geom_text(aes(y=-6,as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1]-0.5,label=paste0("Тижневий огляд інформаційного поля АПУ // ",input$period," // ",format(Sys.Date(), "%d.%m.%Y"))),size=7, lineheight=0.8,hjust=0,vjust=0,family="PT Sans",color="#babdbf")+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-1),size=sqrt(abs(parse_number(unname(unlist(data[2,2:8])))))/i*30,color=
                     ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"#303d7d","#a31e22"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-1.2,label=format(abs(parse_number(unname(unlist(data[2,2:8])))),  big.mark=" ")),color=
                    ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"#303d7d","#a31e22"),family="PT Sans",size=6)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")-0.4,-0.75,label=unlist(unname(sapply(data[3,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),otst), collapse="\n"))))),size=ifelse(nchar(unlist(unname(data[3,2:8])))<100,ifelse(input$typ!="Президент",6,5.5),shrift), lineheight=0.9,hjust=0,vjust=0,family="PT Sans",color=
                    ifelse(parse_number(unname(unlist(data[2,2:8])))>0,"black","#a31e22"))+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-2),size=sqrt(parse_number(unname(unlist(data[4,2:8]))))/i*30,color=
                     ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"#303d7d","#a31e22"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-2.2,label=format(parse_number(unname(unlist(data[4,2:8]))),  big.mark=" ")),color=
                    ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"#303d7d","#a31e22"),family="PT Sans",size=6)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")-0.4,-1.75,label=unlist(unname(sapply(data[5,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),otst), collapse="\n"))))),size=ifelse(nchar(unlist(unname(data[5,2:8])))<100,ifelse(input$typ!="Президент",6,5.5),shrift), lineheight=0.9,hjust=0,vjust=0,family="PT Sans",color=
                    ifelse(parse_number(unname(unlist(data[4,2:8])))>0,"black","#a31e22"))+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-3),size=sqrt(parse_number(unname(unlist(data[10,2:8]))))/i*30,color="#303d7d")+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-3.2,label=ifelse(grepl("NA",format(-parse_number(unname(unlist(data[10,2:8]))))),NA,format(parse_number(unname(unlist(data[10,2:8]))),  big.mark=" "))),color="#303d7d",family="PT Sans",size=6)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")-0.4,-2.75,label=ifelse(unlist(unname(sapply(data[11,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),30), collapse="\n"))))=="NA",NA,unlist(unname(sapply(data[11,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),otst), collapse="\n")))))),color="black",size=ifelse(nchar(unlist(unname(data[11,2:8])))<100,ifelse(input$typ!="Президент",6,5.5),shrift), lineheight=0.9,hjust=0,vjust=0,family="PT Sans")+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-4),size=sqrt((-parse_number(unname(unlist(data[12,2:8])))))/i*30,color="#a31e22")+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-4.2,label=ifelse(grepl("NA",format(-parse_number(unname(unlist(data[12,2:8]))))),NA,format(-parse_number(unname(unlist(data[12,2:8]))),  big.mark=" "))),color="#a31e22",family="PT Sans",size=6)+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")-0.4,-3.75,label=ifelse(unlist(unname(sapply(data[13,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),30), collapse="\n"))))=="NA",NA,unlist(unname(sapply(data[13,2:8],function(x) paste(strwrap(gsub("((","",unname(unlist(x)), fixed="TRUE"),otst), collapse="\n")))))),color="#a31e22",size=ifelse(nchar(unlist(unname(data[13,2:8])))<100,ifelse(input$typ!="Президент",6,5.5),shrift), lineheight=0.9,hjust=0,vjust=0,family="PT Sans")+
        geom_point(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-4.5,size=colSums(rbind(parse_number(unname(unlist(data[2,2:8]))),parse_number(unname(unlist(data[4,2:8]))),parse_number(unname(unlist(data[10,2:8]))),parse_number(unname(unlist(data[12,2:8])))),na.rm = T)),shape=22,color="#babdbf",fill=c("#babdbf","#babdbf","#babdbf","#babdbf","#babdbf","white","white"))+
        geom_text(aes(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30"),-4.6,label=substr(as.character(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")),9,10)),color="#a31e22",family="PT Sans", fontface = "bold",size=6)+
        annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.7, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.55, ymin=-2.2, ymax=-2)+
        annotation_custom(rasterGrob(readPNG("2.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.7, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.55, ymin=-2.6, ymax=-2.4)+
        annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.25, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.15, ymin=-5.4, ymax=-5.25)+
        annotation_custom(g, xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.45, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.35, ymin=-5.4, ymax=-5.25)+
        annotation_custom(rasterGrob(readPNG("2.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.25, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.15, ymin=-5.6, ymax=-5.45)+
        annotation_custom(rasterGrob(readPNG("2.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.45, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.35, ymin=-5.6, ymax=-5.45)+
        annotation_custom(rasterGrob(readPNG("3.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.25, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])+1.15, ymin=-5.75, ymax=-5.65)+
        annotation_custom(rasterGrob(readPNG("3.png"), interpolate=TRUE), xmin=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.45, xmax=as.numeric(as.Date(parse_number(unname(unlist(data[1,2:8]))),origin = "1899-12-30")[1])-0.35, ymin=-5.75, ymax=-5.65)+
        theme_void(base_family="PT Sans")+
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
      filename = function(){paste0(stri_trans_general(input$typ,"latin"),Sys.Date(),".pdf") },
      content = function(file) {
        cairo_pdf(file, width=ifelse(input$typ=="Президент",1875,2222)/100*1.4, height=12.5*1.4)
        print(df())
        dev.off()
      }
    )
    
    output$download<-  downloadHandler(
      filename = function(){paste0(stri_trans_general(input$typ,"latin"),Sys.Date(),".png") },
      content = function(file) {
        png(file, width=ifelse(input$typ=="Президент",1875,2222), height=1250)
        print(df())
        dev.off()
      }
    )
})