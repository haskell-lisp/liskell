def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('exprforms', normal, multimod_compile, ['exprforms',''])
