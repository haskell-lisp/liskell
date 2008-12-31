def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('Bang', normal, multimod_compile, ['Bang',''])

