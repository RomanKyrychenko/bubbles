shinyUI(fluidPage(
  titlePanel("Corestone bubble"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      radioButtons("typ","Оберіть персону",choices = c("Президент","Гройсман","Луценко","Райнин","Ковальчук","Гонтарева",
                                                       "Шимкив","М. Порошенко","Муженко","Ложкин","Геращенко","Елисеев",
                                                       "Филатов","Билоус","Зубко","Терентьев","Хромаев")),
      textInput("period","Введіть період, за який робиться звіт",value = "3-9 квітня 2017"),
      downloadButton('downloadPlot',"Завантажити в pdf!"),
      downloadButton('download',"Завантажити в png!")
    ),
    mainPanel("Візуалізація",plotOutput('plot', width = "2222px", height = "1250px"))
    )
  )
)