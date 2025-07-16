#' @keywords internal
read_GBD <- function(ZIP_folder = FALSE, file_fold = FALSE, foldername = "") {

  if (missing(foldername) || is.null(foldername) || foldername == "" || !file.exists(foldername)) {
    message("未指定有效路径，当前读取的是工作环境下的文件数据")
    foldername <- "."
  }


  if (!ZIP_folder & !file_fold) stop("请至少选择 ZIP_folder 或 file_fold 之一")

  if (ZIP_folder & file_fold) {
    message("同时设置了 ZIP_folder 和 file_fold 为 TRUE，优先读取 ZIP 文件")
    file_fold <- FALSE
  }


  if (ZIP_folder) {
    zipdir <- tempfile()
    dir.create(zipdir)
    zipfiles <- list.files(foldername, full.names = TRUE, pattern = "\\.zip$")
    if (length(zipfiles) == 0) stop("未找到 zip 文件")
    lapply(zipfiles, function(zip) {
      utils::unzip(zip, exdir = zipdir)
    })
    files <- list.files(zipdir, pattern = "\\.csv$", full.names = TRUE)
  } else {

    files <- list.files(foldername, full.names = TRUE, pattern = "\\.csv$")
  }

  if (length(files) == 0) stop("未找到 CSV 文件")


  data_list <- lapply(files, data.table::fread)

  data <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)
  return(data)
}

#' @keywords internal
read_codebooks <- function(folder) {
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  codebook_names <- gsub("\\.csv$", "", basename(files))
  codebook_list <- lapply(files, data.table::fread)
  names(codebook_list) <- codebook_names
  return(codebook_list)
}

#' @keywords internal
id_normalization <- function(data, codebook) {
  data$measure <- codebook[["measure_codebook"]]$measure_name[match(data$measure, codebook[["measure_codebook"]]$measure_id)]
  data$location <- codebook[["location_codebook"]]$location_name[match(data$location, codebook[["location_codebook"]]$location_id)]
  data$sex <- codebook[["sex_codebook"]]$sex_name[match(data$sex, codebook[["sex_codebook"]]$sex_id)]
  data$age <- codebook[["age_codebook"]]$age_name[match(data$age, codebook[["age_codebook"]]$age_id)]
  data$metric <- codebook[["metric_codebook"]]$metric_name[match(data$metric, codebook[["metric_codebook"]]$metric_id)]
  if ("cause" %in% names(data)) data$cause <- codebook[["cause_codebook"]]$cause_name[match(data$cause, codebook[["cause_codebook"]]$cause_id)]
  if ("rei" %in% names(data)) data$rei <- codebook[["rei_codebook"]]$rei_name[match(data$rei, codebook[["rei_codebook"]]$rei_id)]
  return(data)
}

#' @keywords internal
location_normalization <- function(data, trans_location) {
  data$location <- sub("Türkiye", "Turkey", data$location)
  data$location <- ifelse(data$location %in% trans_location$location,
                          trans_location$normalized_location[match(data$location, trans_location$location)],
                          data$location)
  return(data)
}

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
zstatsreadGBD <- function(ZIP_folder = FALSE, file_fold = TRUE, foldername = "") {
  raw_data <- read_GBD(ZIP_folder, file_fold, foldername)

  if (length(grep("id", names(raw_data))) > 1) {
    if (any(grepl("start", names(raw_data)))) {
      raw_data <- as.data.frame(raw_data)[, unique(c(
        grep("id|start|end", names(raw_data)),
        (ncol(raw_data) - 3):ncol(raw_data)
      ))]
    } else {
      raw_data <- as.data.frame(raw_data)[, c(
        grep("id", names(raw_data)),
        (ncol(raw_data) - 3):ncol(raw_data)
      )]
    }
    names(raw_data) <- gsub("_id", "", names(raw_data))
  }

  # data("zstatscodebook", envir = environment())
  # data("zstatslocation", envir = environment())
  codebook <- zstatscodebook
  trans_location <- zstatslocation

  if (is.numeric(raw_data$measure)) {
    data_std <- id_normalization(raw_data, codebook)
  } else {
    data_std <- raw_data
  }

  data_final <- location_normalization(data_std, trans_location)
  return(data_final)
}
