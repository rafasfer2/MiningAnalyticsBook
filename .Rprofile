## Project .Rprofile: fix miningKPI dataset `cycle` list-column for non-interactive renders
local({
    try(
        {
            if (requireNamespace("miningKPI", quietly = TRUE)) {
                data_name <- "drill_events_mine_d"
                env <- new.env()
                tryCatch(
                    {
                        utils::data(list = data_name, package = "miningKPI", envir = env)
                        if (exists(data_name, envir = env)) {
                            df <- get(data_name, envir = env)
                            if (!is.null(df) && is.list(df$cycle)) {
                                df$cycle <- vapply(df$cycle, function(x) {
                                    if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x[1])
                                }, integer(1))
                                assign(data_name, df, envir = .GlobalEnv)
                            }
                        }
                    },
                    error = function(e) {}
                )
            }
        },
        silent = TRUE
    )
})
source("renv/activate.R")
