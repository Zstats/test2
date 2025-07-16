#' GBD数据读取与处理
#'
#' 一键读取、合并数据形成GBD数据框
#'
#' @param ZIP_folder 是否读取zip压缩包（若读取zip压缩包，ZIP_folder = TRUE，否则ZIP_folder = FALSE）
#' @param file_fold 是否读取csv文件夹（若读取csv文件，则file_fold = TRUE，否则file_fold = FALSE）
#' @param Parallel 是否启用并行读取csv （若想快速读取，则Parallel = TRUE，否则Parallel = FALSE）
#' @param foldername 默认读取工作环境下的zip或csv，若想读取其他文件夹下zip或csv文件，请设置工作路径
#'
#' @return 返回一个合并后的GBD数据框，包含以下变量：
#' @return measure：疾病负担衡量指标，包含发病（Incidence）,患病（Prevalence）,死亡（Deaths）,残疾调整生命年DALYs（Disability-adjusted life years）,伤残损失的寿命年 YLDs (Years Lived with Disability)，寿命损失年YLLs (Years of Life Lost)，总暴露值SEV（Summary exposure value）
#' @return location：全球区域、地区或国家的名称
#' @return sex：性别，包含男性Male,女性Female,两性Both
#' @return age：年龄段
#' @return cause：疾病名称
#' @return metric：数据类型，包含Number,Rate,Percent
#' @return year：年份
#' @return val：值，即疾病负担
#' @return upper：上95%UI区间值
#' @return lower：下95%UI区间值
#' @return rei：风险因素变量，即risk factor
#' @export

zstatsreadGBD <- function(ZIP_folder = FALSE, file_fold = TRUE, foldername = ""){
  # 采用compiler 方式加载
  # fun_env <- new.env()
  # compiler::loadcmp("inst/bytecode/utils.rds", envir = fun_env)
  # utilTool <-fun_env$UtilsTool$new()

  #utilTool <-UtilsTool$new()

  # return(utilTool$med_cal(beta1,se1,beta2,se2,beta_total,n))
  args <- list(ZIP_folder , file_fold, foldername)

  # 使用加密文件路径
  enc_path <- system.file("bytecode", "GBDread.enc", package = "ZGBD")
  result <-.Call("call_bytecode_func", as.character("zstatsreadGBD"), args, enc_path)
  return(result)
}

