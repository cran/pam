#' Read and Process DualPAM Data
#'
#' Reads raw CSV files generated by DualPAM software, calculates electron transport rate (ETR) values, and returns a cleaned dataset. Customization may be needed for non-DualPAM devices.
#'
#' @param csv_path File path to the CSV file.
#' @param remove_recovery Logical. Removes recovery measurements if \code{TRUE}. Default is \code{TRUE}.
#' @param etr_factor Numeric. Factor for ETR calculation. Default is \code{0.84}.
#' @param fraction_photosystem_I Numeric. Relative distribution of absorbed PAR to photosystem I. Default is \code{0.5}.
#' @param fraction_photosystem_II Numeric. Relative distribution of absorbed PAR to photosystem II. Default is \code{0.5}.
#'
#' @details
#' Calculates ETR using:
#' \deqn{\text{ETR} = \text{PAR} \cdot \text{ETR-Factor} \cdot \text{Fraction of Photosystem (I or II)} \cdot \text{Yield (I or II)}}
#'
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#read_dual_pam_data}
#'
#' @return A `data.table` with processed data and calculated ETR values.
#'
#' @references{
#'   Heinz Walz GmbH. (2024). \emph{DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).}
#'   Heinz Walz GmbH, Effeltrich, Germany.
#'   Available at: \url{https://www.walz.com/files/downloads/manuals/dual-pam-100/DualPamEd05.pdf}
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#' @export
read_dual_pam_data <- function(
    csv_path,
    remove_recovery = TRUE,
    etr_factor = 0.84,
    fraction_photosystem_I = 0.5,
    fraction_photosystem_II = 0.5) {
  if (fraction_photosystem_I + fraction_photosystem_II != 1) {
    stop("The sum of fraction_photosystem_I and fraction_photosystem_II must be equal 1.")
  }

  tryCatch(
    {
      data <- utils::read.csv(csv_path, sep = ";", dec = ".")
      data <- data.table::as.data.table(data)

      validate_data(data)
      data <- data[data$ID == "SP", ]

      date_time_col_values <- c()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]

        date_time_row_value <- as.POSIXct(
          paste(row$Date, row$Time, sep = " "),
          tz = "GMT", "%d.%m.%y %H:%M:%S"
        )
        date_time_col_values <- c(date_time_col_values, date_time_row_value)
      }

      data <- dplyr::mutate(data, DateTime = date_time_col_values)
      data <- data[order(data$DateTime), ]

      result <- data.table::data.table()
      last_par <- as.numeric(0)
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        current_par <- row$PAR

        if (remove_recovery && last_par != 0 && current_par < last_par) {
          break
        }

        yield_I <- row$Y.I.
        recalc_ETRI <- calc_etr(yield_I, current_par, etr_factor, fraction_photosystem_I)
        row <- cbind(row, etr_I_col_name = recalc_ETRI)
        data.table::setnames(row, old = "etr_I_col_name", new = etr_I_type)

        yield_II <- row$Y.II.
        recalc_ETRII <- calc_etr(yield_II, current_par, etr_factor, fraction_photosystem_II)
        row <- cbind(row, etr_II_col_name = recalc_ETRII)
        data.table::setnames(row, old = "etr_II_col_name", new = etr_II_type)

        result <- rbind(result, row)

        last_par <- current_par
      }

      return(result)
    },
    warning = function(w) {
      stop("Warning in file: ", csv_path, " Warning: ", w)
    },
    error = function(e) {
      stop("Error in file: ", csv_path, " Error: ", e)
    }
  )
}

calc_etr <- function(yield, par, etr_factor, p_ratio) {
  if (is.na(yield)) {
    return(NA_real_)
  }

  if (!is.numeric(yield)) {
    stop("yield is not numeric")
  }

  if (!is.numeric(par)) {
    stop("par is not numeric")
  }

  if (!is.numeric(etr_factor)) {
    stop("etr_factor is not numeric")
  }

  if (!is.numeric(p_ratio)) {
    stop("p_ratio is not numeric")
  }

  return(yield * par * etr_factor * p_ratio)
}
