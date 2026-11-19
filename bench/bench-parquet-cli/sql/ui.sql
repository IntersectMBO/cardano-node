-- DuckDB UI for a query session (query --ui). Loaded last, after all views.
-- INSTALL downloads the extension only if it isn't installed yet (once per DuckDB
-- version, into ~/.duckdb/extensions); LOAD is needed in every process, because
-- this DuckDB build doesn't autoload extensions. start_ui() starts the UI server
-- on localhost and opens it in a browser tab.
INSTALL ui;
LOAD ui;
CALL start_ui();
