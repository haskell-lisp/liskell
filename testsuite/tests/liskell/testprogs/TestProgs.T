def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('Factorial', normal, multimod_compile_and_run, ['Factorial',''])
test('TicTacToe', normal, multimod_compile_and_run, ['TicTacToe','-main-is TicTacToe'])
test('Star', normal, multimod_compile_and_run, ['Star','-main-is Star -package LskPrelude'])
test('Bezier', extra_run_opts('macro'), multimod_compile_and_run, ['Bezier','-main-is Bezier -package LskPrelude'])
test('Bezier', extra_run_opts('fun'), multimod_compile_and_run, ['Bezier','-main-is Bezier -package LskPrelude'])
