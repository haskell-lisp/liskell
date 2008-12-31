def f(opts):
	opts.liskell = 1

setTestOpts(f)

test('TypeForms', normal, multimod_compile, ['TypeForms',''])
