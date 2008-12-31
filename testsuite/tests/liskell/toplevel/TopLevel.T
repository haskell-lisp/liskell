def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('Test01', normal, multimod_compile, ['Test01',''])
test('Test02', normal, multimod_compile, ['Test02','-main-is Test02'])


test('NewType', normal, multimod_compile_and_run, ['NewType','-main-is NewType'])
test('Data', normal, multimod_compile_and_run, ['Data','-main-is Data'])
test('Class', normal, multimod_compile_and_run, ['Class','-main-is Class'])



