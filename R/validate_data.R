## Validação e coerção de datasets do miningKPI usados pelo livro
## Este script garante que colunas que porventura sejam list-columns
## (ex.: `cycle`) sejam convertidas para vetores atômicos inteiros.

validate_miningKPI_data <- function() {
    if (!requireNamespace("miningKPI", quietly = TRUE)) {
        return(invisible(NULL))
    }
    data_name <- "drill_events_mine_d"
    env <- new.env()
    tryCatch(
        {
            utils::data(list = data_name, package = "miningKPI", envir = env)
            if (exists(data_name, envir = env)) {
                df <- get(data_name, envir = env)
                if (is.data.frame(df) && !is.null(df$cycle) && is.list(df$cycle)) {
                    df$cycle <- vapply(df$cycle, function(x) {
                        if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x[1])
                    }, integer(1))
                    assign(data_name, df, envir = .GlobalEnv)
                }
            }
        },
        error = function(e) {
            message("validate_miningKPI_data: erro ao validar dados: ", conditionMessage(e))
        }
    )
    invisible(NULL)
}

# Executa automaticamente quando o arquivo é sourced
validate_miningKPI_data()
