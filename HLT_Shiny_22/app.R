# Load necessary libraries
library(shiny)
library(hlt.falcon)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("FDA Table 02"),

  # Sidebar with file upload and download options
  sidebarLayout(
    sidebarPanel(
      # File upload input
      fileInput("upload", "Upload files", multiple = TRUE),
      # Table type selection input
      selectInput("table_type", "Table Type", choices = "02"),
      # Download button
      downloadButton("download", "Download rtf")
    ),

    # Main panel with table output
    mainPanel(
      tableOutput("datatbl")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive object that reads the uploaded data
  data <- reactive({
    req(input$upload)
    data_list <- lapply(input$upload$datapath, haven::read_sas)
    names(data_list) <- input$upload$name
    data_list
  })

  # Reactive object that creates the table
  table <- reactive({
    req(input$upload)
    stopifnot(any(tolower(input$upload$name) == "adsl.sas7bdat"), any(tolower(input$upload$name) == "advs.sas7bdat"))
    adsl_path <- input$upload$datapath[tolower(input$upload$name) == "adsl.sas7bdat"]
    advs_path <- input$upload$datapath[tolower(input$upload$name) == "advs.sas7bdat"]
    tryCatch({
      adsl <- haven::read_sas(adsl_path)
      advs <- haven::read_sas(advs_path)
    }, error = function(e) {
      stop("Error reading files: ", e$message)
    })
    create_table_02(
      df = advs, 
      adsl = adsl,
      vars = c("AGE", "SEX", "ETHNICC", "HEIGHTBL", "WEIGHTBL","BMIBL"),
      lbl_vars = c(
        AGE = "年龄（岁）",
        SEX = "性别, n (%)",
        ETHNICC = "民族, n (%)",
        HEIGHTBL = "身高（cm）",
        WEIGHTBL = "体重（kg）",
        BMIBL = "基线体重指数（kg/m2）"
      ),
      .labels = c(n = "例数 (缺失)", mean_sd = "均值(标准差)", median = "中位数", quantiles = "Q1. Q3", range = "最小值. 最大值", count_fraction = "n (%)")
    )
  })
  
  # Render the table in the main panel
  output$datatbl <- renderTable({
    table()
  })

  # Handle file download
  output$download <- downloadHandler(
    filename = function() {
      "fda-table-02.rtf"
    },
    content = function(file) {
      df_tbl <- table()
      column_labels <- c("", "空腹剂量组 100mg \n(N=6)", "空腹剂量组 200mg \n(N=6)", "空腹剂量组 400mg \n(N=6)", "空腹剂量组 800mg \n(N=6)", "餐后剂量组 400mg \n(N=6)", "餐后剂量组 800mg \n(N=6)", "安慰剂 \n(N=12)", "合计 \n(N=48)")
      # Create a report with the table
      report_path <- create_sassy_report(
        df_tbl = df_tbl,
        headers = "用药前后基线情况-HLT",
        titles = "表格14.3.4.2 用药前后体格检查参数临床评估的交叉表 — 安全性分析集",
        footnotes = c("数据来源：列表16.2.x", "注：百分比计算的分母基于安全性分析集的受试者人数。", "[1] 依从性 = 实际用药/计划用药量x100%。"),
        page_header = c("[Mock-up TLF shells (CN)", "Statistical Analysis Tables and Figures, List (Chinese Version)]", "[TP-HLT-BS-004,V1.0,15Mar2024]"),
        page_footer = c("Associated Process:", "关联流程：SOP-HLT-BS-001", "Confidentiality保密"),
        column_labels = column_labels,
        file = file
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)