% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBDplottwo.R
\name{zstatsPlotNumberRate}
\alias{zstatsPlotNumberRate}
\title{GBD 双轴绘图函数（条形图 + 折线图）}
\usage{
zstatsPlotNumberRate(
  data,
  x_axis = "year",
  ratio = "auto",
  group = "sex",
  ASR = TRUE,
  groupcolor = "a",
  group_colors = NULL,
  show_CI = TRUE,
  ylabel_left = NULL,
  ylabel_right = NULL,
  xlab = NULL,
  legend_title_group = NULL,
  x_breaks = NULL
)
}
\arguments{
\item{data}{数据框(zstatsreadGBD()函数生成的GBD数据框或使用zstatsfilter()函数筛选后生成的数据框)，需包含 \code{val}、\code{metric}、\code{age}、\code{year}、分组变量(如sex或location)等列}

\item{x_axis}{横轴变量，可选 \code{"year"} 或 \code{"age"}，默认 \code{"year"}}

\item{ratio}{条形图与折线图之间的缩放比例；可设为 \code{"auto"}(默认)或数值(相对于"auto"比例的倍数，如ratio = 0.8,即表示缩放比例为"auto"时的比例的0.8倍)}

\item{group}{分组变量列名（如 "sex"、"location"），默认 \code{"sex"}}

\item{ASR}{是否优先使用 Age-standardized（ASR）率，默认 TRUE；否则使用所有年龄的 Rate,即粗率}

\item{groupcolor}{内置颜色方案选择（"a"、"b"、"c"），默认为 "a"}

\item{group_colors}{自定义颜色命名向量，优先于 \code{groupcolor}，默认 NULL}

\item{show_CI}{是否显示置信区间，默认 TRUE(要求数据包含 \code{lower} 和 \code{upper} 列)}

\item{ylabel_left}{左轴(数量)的 y 轴标签，默认自动生成}

\item{ylabel_right}{右轴(比率)的 y 轴标签，默认自动生成}

\item{xlab}{x 轴标签，默认自动生成}

\item{legend_title_group}{图例标题前缀，默认使用 \code{group} 名称}

\item{x_breaks}{用于控制 x 轴密度,设置 x 轴显示的刻度(例如年份 x_breaks = 1990:2021)，向量形式}
}
\value{
ggplot 对象
}
\description{
用于绘制 GBD 数据中“数量”与“率”同时呈现的双轴图，左轴显示数量（Number），右轴显示比率（ASR 或 Rate），适用于不同分组变量（如性别、地区）。
如果绘制的以性别(sex)分组的双轴图,则你的数据框中需包含measure(仅含一种),location(仅含一种),sex,age,cause(仅含一种),rei(若有，仅含一种),metric,year,val,lower,upper。
}
