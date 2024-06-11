安装 falcon 包

```R
if (!require("formatters")) install.packages("formatters")
if (!require("rtables")) install.packages("rtables")
if (!require("rlistings")) install.packages("rlistings")
if (!require("remotes")) install.packages("remotes")
if (!require("tern")) install.packages("tern")

if (!require("falcon")) remotes::install_github("pharmaverse/falcon")
```

> [!Note]
>通过此方法安装的 falcon 包版本应为：0.1.0.9055

安装 hlt.falcon 包

```r
remotes::install_github("hlt-stat/hlt.falcon")
```

> [!Note]
> 需要注意的是，在安装 hlt.falcon 包时，如果提示 falcon 包有更新的版本「0.2.0」，不要更新！

安装完成后，使用以下代码进行测试：

```R
# 设置数据集
data <- load_libraries_and_data()

# 创建表 table_22
table_22 <- create_table_22(df = data$df, adsl = data$adsl)

# 输出表 table_22
print(table_22)

# 在当前目录下导出表格
report_table <- create_sassy_report(
  df_tbl = table_22,
  headers = "用药前后基线情况-HLT",
  titles = "表格14.3.4.2 用药前后体格检查参数临床评估的交叉表 — 安全性分析集",
  footnotes = c("数据来源：列表16.2.x", "注：百分比计算的分母基于安全性分析集的受试者人数。", "[1] 依从性 = 实际用药/计划用药量x100%。"),
  page_header = c("[Mock-up TLF shells (CN)", "Statistical Analysis Tables and Figures, List (Chinese Version)]", "[TP-HLT-BS-004,V1.0,15Mar2024]"),
  page_footer = c("Associated Process:", "关联流程：SOP-HLT-BS-001", "Confidentiality保密"),
  column_labels = c("访视 \n治疗组", "正常 \nN = 134", "异常无临床意义 \nN = 134", "异常有临床意义 \nN = 132"),
  file = "fda-table-22.rtf"
)
```