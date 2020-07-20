header <- dashboardHeader(
    title = "Data Proyeksi Pertumbuhan Penduduk dan Kemiskinan Jabar 2010 - 2016",
    titleWidth = 670
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text = "Proyeksi Penduduk",
            tabName = "proyeksi",
            icon = icon("bar-chart")
        ),
        menuItem(
            text = "Persentasi Penduduk Miskin",
            tabName = "persentasi",
            icon = icon("bar-chart")
        ),
        menuItem(
            text = "Tabular Data",
            tabName = "tabular",
            icon = icon("table")
        ),
        menuItem(
            text = "Source Data",
            tabName = "source",
            icon = icon("link")
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "proyeksi",
            fluidRow(
                valueBoxOutput("progressBox"),
                valueBoxOutput("statusBox"),
                valueBoxOutput("jumlahBox")
            ),
            fluidRow(
                column(
                    width = 8,
                    plotlyOutput("pertumbuhanPendJabar")
                ),
                column(
                    width = 4,
                    selectInput(
                        inputId = "selectKab",
                        label = "Select Kabupaten/Kota",
                        choices = unique(jml_pend_clean$nama_kota_kabupaten)
                    )
                )
            ),
            br(),
            fluidRow(
                column(
                    width = 8,
                    plotlyOutput("proyeksiJmlPend")
                ),
                column(
                    width = 4,
                    selectInput(
                        inputId = "selectYear",
                        label = "Select Kabupaten/Kota",
                        choices = unique(jml_pend_clean$tahun)
                    )
                )
            )
        ),
        tabItem(
            tabName = "persentasi",
            fluidRow(
                column(
                    selectInput(
                        inputId = "selectPendMiskin",
                        label = "Select Tahun",
                        choices = unique(pend_clean$tahun)
                    ),
                    width = 6,
                    plotlyOutput("pendMiskin")
                ),
                column(
                    selectInput(
                        inputId = "selectMiskinKab",
                        label = "Select Kab/Kota",
                        choices = unique(pend_clean$nama_kota_kabupaten)
                    ),
                    width = 6,
                    plotlyOutput("grafikPendMiskin")
                )
            ),
            br(),
            fluidRow(
                column(
                    width = 12,
                    plotlyOutput("pendMiskinAndSejahtera")
                )
            )
        ),
        tabItem(
            tabName = "tabular",
            h2("Data Proyeksi Penduduk Jabar 2010 - 2016"),
            DT::dataTableOutput("dataPendJabar"),
            br(),
            h2("Data Persentase Jumlah Penduduk Miskin Jabar 2010-2016"),
            DT::dataTableOutput("dataPendMiskin")


        ),
        tabItem(
            tabName = "source",
            h2("LINK"),
            fluidPage(
                uiOutput("tab1"),
                uiOutput("tab2")
            )
        )
    )
)

dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)