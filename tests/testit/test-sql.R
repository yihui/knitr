library(testit)

assert(
  !is_sql_update_query('SELECT 1'),
  !is_sql_update_query('SELECT * FROM foo'),
  !is_sql_update_query(' SELECT 1'),
  !is_sql_update_query('\nSELECT 1'),
  !is_sql_update_query('\tSELECT 1')
)

assert(!is_sql_update_query(c('-- Some SQL', 'SELECT 1')))

assert(!is_sql_update_query(c('/* ', '   Some SQL', '*/', 'SELECT 1')))

assert(!is_sql_update_query(c('   /* ', '      Some SQL', '   */', 'SELECT 1')))

assert(is_sql_update_query('UPDATE foo SET a=1'))
assert(is_sql_update_query(' UPDATE foo SET a=1'))
assert(is_sql_update_query('\n\nUPDATE foo SET a=1'))
assert(is_sql_update_query('\tUPDATE foo SET a=1'))
assert(is_sql_update_query('DELETE FROM foo'))
assert(is_sql_update_query('INSERT INTO foo values(1)'))

assert(is_sql_update_query(c('-- SELECT 1', 'INSERT INTO foo values(1)')))

assert(is_sql_update_query(c('/*SELECT 1*/', '   INSERT INTO foo values(1)')))

assert(is_sql_update_query(c(
  '/*', '   Insert records into table', '*/', '', '   INSERT INTO foo values(1)'
)))

assert(is_sql_update_query('update foo set a=1'))
assert(is_sql_update_query('delete from foo'))
assert(is_sql_update_query('insert into foo values(1)'))
