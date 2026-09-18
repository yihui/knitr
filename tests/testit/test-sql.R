library(testit)

assert('validation of sql queries works', {
  (!is_sql_update_query('SELECT 1'))
  (!is_sql_update_query('SELECT * FROM foo'))
  (!is_sql_update_query(' SELECT 1'))
  (!is_sql_update_query('\nSELECT 1'))
  (!is_sql_update_query('\tSELECT 1'))
  (!is_sql_update_query(c('-- Some SQL', 'SELECT 1')))
  (!is_sql_update_query(c('/* ', '   Some SQL', '*/', 'SELECT 1')))
  (!is_sql_update_query(c('   /* ', '      Some SQL', '   */', 'SELECT 1')))

  (is_sql_update_query('UPDATE foo SET a=1'))
  (is_sql_update_query(' UPDATE foo SET a=1'))
  (is_sql_update_query('\n\nUPDATE foo SET a=1'))
  (is_sql_update_query('\tUPDATE foo SET a=1'))
  (is_sql_update_query('DELETE FROM foo'))
  (is_sql_update_query('INSERT INTO foo values(1)'))

  (is_sql_update_query(c('-- SELECT 1', 'INSERT INTO foo values(1)')))

  (is_sql_update_query(c('/*SELECT 1*/', '   INSERT INTO foo values(1)')))

  (is_sql_update_query(c(
    '/*', '   Insert records into table', '*/', '', '   INSERT INTO foo values(1)'
  )))

  (is_sql_update_query('update foo set a=1'))
  (is_sql_update_query('delete from foo'))
  (is_sql_update_query('insert into foo values(1)'))

  # additional DDL/DML keywords recognized as statements
  (is_sql_update_query('alter table foo add column x (int);'))
  (is_sql_update_query('merge into table foo from (select x, y from bar) as bar2 on foo.x = bar.x when matched then update set foo.y = bar.y;'))
  (is_sql_update_query('grant select on foo to user1'))
  (is_sql_update_query('deny select on foo to user1'))
  (is_sql_update_query('revoke select on foo to user1'))
  (is_sql_update_query('analyze table foo.bar compute statistics for all columns'))
  (is_sql_update_query('audit select on hr.employees whenever successful'))
  (is_sql_update_query("comment on column foo.bar is 'hello world'"))
  (is_sql_update_query('rename table foo to bar;'))
  (is_sql_update_query('truncate table foo'))
  (is_sql_update_query("call sysproc.admin_cmd('REORG TABLE foo')"))
  (is_sql_update_query('explain plan for select * from foo where bar > 0'))
  (is_sql_update_query('lock table foo in exclusive mode nowait;'))
  (is_sql_update_query('unlock tables'))
})

assert('split_sql() splits statements on top-level semicolons (#2093)', {
  # basic splitting and whitespace trimming
  (split_sql('SELECT 1; SELECT 2') %==% c('SELECT 1', 'SELECT 2'))
  (split_sql('SELECT 1;') %==% 'SELECT 1')
  (split_sql('SELECT 1') %==% 'SELECT 1')
  # empty statements are dropped
  (split_sql('SELECT 1;;; SELECT 2;') %==% c('SELECT 1', 'SELECT 2'))
  (split_sql('   \n  ') %==% character(0))
  # multi-line input from a vector is joined first
  (split_sql(c('SELECT 1 AS A;', "SELECT 2 AS A, 'B' AS B")) %==%
     c('SELECT 1 AS A', "SELECT 2 AS A, 'B' AS B"))

  # semicolons inside string literals are not split points
  (split_sql("SELECT ';' AS x; SELECT 2") %==% c("SELECT ';' AS x", 'SELECT 2'))
  # doubled single quote is an escaped quote inside a string
  (split_sql("SELECT 'it''s' ; SELECT 2") %==% c("SELECT 'it''s'", 'SELECT 2'))
  # quoted identifiers ("..." and `...`) are also protected
  (split_sql('SELECT "a;b" FROM t; SELECT 2') %==% c('SELECT "a;b" FROM t', 'SELECT 2'))
  (split_sql('SELECT `a;b` FROM t; SELECT 2') %==% c('SELECT `a;b` FROM t', 'SELECT 2'))

  # semicolons inside comments are ignored (-- , # , and /* */)
  (split_sql('SELECT 1 -- foo; bar\n; SELECT 2') %==% c('SELECT 1 -- foo; bar', 'SELECT 2'))
  (split_sql('SELECT 1 # foo; bar\n; SELECT 2') %==% c('SELECT 1 # foo; bar', 'SELECT 2'))
  (split_sql('SELECT 1 /* a; b */; SELECT 2') %==% c('SELECT 1 /* a; b */', 'SELECT 2'))
})

if (all(vapply(c('DBI', 'RSQLite'), loadable, logical(1)))) {
  con = DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  opts_knit$set(out.format = 'markdown')
  run_sql = function(code, ...) {
    o = opts_chunk$merge(list(
      engine = 'sql', connection = con, label = 'test-sql', code = code, ...
    ))
    one_string(knitr:::eng_sql(o))
  }

  assert('sql.interlaced runs each statement and emits its own result (#2093)', {
    out = run_sql(c('SELECT 1 AS A;', "SELECT 2 AS A, 'B' AS B"), sql.interlaced = TRUE)
    # both statements are echoed as separate source blocks
    (grepl('SELECT 1 AS A', out) && grepl("SELECT 2 AS A, 'B' AS B", out))
    # both results are rendered (the second statement is not dropped)
    (grepl('\\|[ ]*1\\|', out) && grepl('\\|B[ ]*\\|', out))
  })

  assert('sql.interlaced stops at the first failing statement when error = TRUE', {
    out = run_sql(c('SELECT 1 AS A;', 'SELECT * FROM nonesuch'),
                  sql.interlaced = TRUE, error = TRUE)
    (grepl('SELECT 1 AS A', out))          # first statement still shown
    (grepl('no such table', out))          # error surfaced for the second
  })

  assert('sql.interlaced with a single statement behaves like the normal engine', {
    a = run_sql('SELECT 1 AS A', sql.interlaced = TRUE)
    b = run_sql('SELECT 1 AS A')
    (a %==% b)
  })

  assert('output.var captures a list of results in sql.interlaced mode (#2093)', {
    invisible(run_sql(c('SELECT 1 AS A;', 'SELECT 2 AS B'),
                      sql.interlaced = TRUE, output.var = 'res_list'))
    res = get('res_list', envir = knit_global())
    (is.list(res) && length(res) == 2L)
    (res[[1]][['A']] %==% 1L && res[[2]][['B']] %==% 2L)
  })

  DBI::dbDisconnect(con)
  opts_knit$restore()
}
