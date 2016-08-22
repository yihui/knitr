library(testit)

assert(identical(is_sql_update_query("SELECT 1"), FALSE))
assert(identical(is_sql_update_query("SELECT * FROM foo"), FALSE))
assert(identical(is_sql_update_query(" SELECT 1"), FALSE))
assert(identical(is_sql_update_query("\nSELECT 1"), FALSE))
assert(identical(is_sql_update_query("\tSELECT 1"), FALSE))

assert(identical(is_sql_update_query(paste(
  "-- Some SQL",
  "SELECT 1",
  sep = "\n")), FALSE))

assert(identical(is_sql_update_query(paste(
  "/* ",
  "   Some SQL",
  "*/",
  "SELECT 1",
  sep = "\n")), FALSE))

assert(identical(is_sql_update_query(paste(
  "   /* ",
  "      Some SQL",
  "   */",
  "SELECT 1",
  sep = "\n")), FALSE))

assert(identical(is_sql_update_query("UPDATE foo SET a=1"), TRUE))
assert(identical(is_sql_update_query(" UPDATE foo SET a=1"), TRUE))
assert(identical(is_sql_update_query("\n\nUPDATE foo SET a=1"), TRUE))
assert(identical(is_sql_update_query("\tUPDATE foo SET a=1"), TRUE))
assert(identical(is_sql_update_query("DELETE FROM foo"), TRUE))
assert(identical(is_sql_update_query("INSERT INTO foo values(1)"), TRUE))

assert(identical(is_sql_update_query(paste(
  "-- SELECT 1",
  "INSERT INTO foo values(1)",
  sep = "\n")), TRUE))

assert(identical(is_sql_update_query(paste(
  "/*SELECT 1*/",
  "   INSERT INTO foo values(1)",
  sep = "\n")), TRUE))

assert(identical(is_sql_update_query(paste(
  "/*",
  "   Insert records into table",
  "*/",
  "",
  "   INSERT INTO foo values(1)",
  sep = "\n")), TRUE))

assert(identical(is_sql_update_query("update foo set a=1"), TRUE))
assert(identical(is_sql_update_query("delete from foo"), TRUE))
assert(identical(is_sql_update_query("insert into foo values(1)"), TRUE))
