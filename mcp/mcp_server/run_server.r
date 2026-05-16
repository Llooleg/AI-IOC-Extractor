# Запуск MCP Plumber сервера.
# Запускать из корня проекта: Rscript mcp_server/run_server.r

library(here)
library(plumber)

if (file.exists(here(".Renviron"))) {
  readRenviron(here(".Renviron"))
}

port <- as.integer(Sys.getenv("MCP_SERVER_PORT", unset = "8000"))

cli::cli_h1("MCP Сервер (R Plumber)")
cli::cli_inform("Endpoint : {Sys.getenv('ENDPOINT')}")
cli::cli_inform("Модель   : {Sys.getenv('AI_MODEL', unset = 'grok-4.3')}")
cli::cli_inform("Порт     : {port}")
cli::cli_inform("MongoDB  : {Sys.getenv('MONGO_HOST')}")
cli::cli_rule()

plumber::pr(here("mcp_server", "plumber.r")) |>
  plumber::pr_run(
    host = "0.0.0.0",
    port = port,
    docs = FALSE
  )
