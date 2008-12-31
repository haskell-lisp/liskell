def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('patternforms', normal, multimod_compile, ['patternforms',''])




