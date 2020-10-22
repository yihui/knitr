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
assert(is_sql_update_query('alter table foo add column x (int);'))
assert(is_sql_update_query('merge into table foo from (select x, y from bar) as bar2 on foo.x = barr.x when matched then update set foo.y = bar.y;'))
assert(is_sql_update_query('grant select on foo to user1'))
assert(is_sql_update_query('deny select on foo to user1'))
assert(is_sql_update_query('revoke select on foo to user1'))
assert(is_sql_update_query('analyze table foo.bar compute statistics for all columns'))
assert(is_sql_update_query('audit select on hr.employees whenever successful'))
assert(is_sql_update_query("comment on column foo.bar is 'hello world'"))
assert(is_sql_update_query('rename table foo to bar;'))
assert(is_sql_update_query('truncate table foo'))
assert(is_sql_update_query("call sysproc.admin_cmd('REORG TABLE foo')"))
assert(is_sql_update_query('explain plan for select * from foo where bar > 0'))
assert(is_sql_update_query('lock table foo in exclusive mode nowait;'))
assert(is_sql_update_query('unlock tables'))




