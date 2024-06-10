# hlt.falcon

## Installation
您可以通过运行以下命令从 GitHub 直接安装`hlt.falcon`最新开发版本并从 CRAN 安装其依赖项：

```
if (!require("formatters")) install.packages("formatters")
if (!require("rtables")) install.packages("rtables")
if (!require("rlistings")) install.packages("rlistings")
if (!require("tern")) install.packages("tern")
if (!require("falcon")) install.packages("falcon")

if (!require("remotes")) install.packages("remotes")
remotes::install_github("hlt-stat/hlt.falcon")
```

## Usage
1. 首先加载 `hlt.falcon` 包，然后加载你需要使用的数据。可以使用 load_libraries_and_data 函数加载数据：
```
library(hlt.falcon)
data <- load_libraries_and_data()
```



2. 接下来，你可以使用 create_table_22 函数创建 Table 22 数据框：
```
table_22 <- create_table_22(data$df, data$adsl)
```

3. 然后，使用 create_sassy_report 函数生成 RTF 报告。确保你已经准备好页眉，页脚，标题和其他相关参数：

```
report_path <- create_sassy_report(
  df_tbl = table_22,
  titles = "Your report title",
  footnotes = "Your report footnotes",
  headers = "Your report headers",
  page_header = "Your page header",
  page_footer = "Your page footer",
  column_labels = c("Column 1 Label", "Column 2 Label", "Column 3 Label", "Column 4 Label"),
  file = "your-report-file-name.rtf"
)
```