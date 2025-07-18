% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBDread.R
\name{zstatsreadGBD}
\alias{zstatsreadGBD}
\title{GBD数据读取与处理}
\usage{
zstatsreadGBD(
  ZIP_folder = FALSE,
  file_fold = TRUE,
  Parallel = FALSE,
  foldername = ""
)
}
\arguments{
\item{ZIP_folder}{是否读取zip压缩包（若读取zip压缩包，ZIP_folder = TRUE，否则ZIP_folder = FALSE）}

\item{file_fold}{是否读取csv文件夹（若读取csv文件，则file_fold = TRUE，否则file_fold = FALSE）}

\item{Parallel}{是否启用并行读取csv （若想快速读取，则Parallel = TRUE，否则Parallel = FALSE）}

\item{foldername}{默认读取工作环境下的zip或csv，若想读取其他文件夹下zip或csv文件，请设置工作路径}
}
\value{
返回一个合并后的GBD数据框，包含以下变量：

measure：疾病负担衡量指标，包含发病（Incidence）,患病（Prevalence）,死亡（Deaths）,残疾调整生命年DALYs（Disability-adjusted life years）,伤残损失的寿命年 YLDs (Years Lived with Disability)，寿命损失年YLLs (Years of Life Lost)，总暴露值SEV（Summary exposure value）

location：全球区域、地区或国家的名称

sex：性别，包含男性Male,女性Female,两性Both

age：年龄段

cause：疾病名称

metric：数据类型，包含Number,Rate,Percent

year：年份

val：值，即疾病负担

upper：上95\%UI区间值

lower：下95\%UI区间值

rei：风险因素变量，即risk factor
}
\description{
一键读取、合并数据形成GBD数据框
}
