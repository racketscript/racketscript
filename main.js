import * as Expander from './expander.js';
import * as CORE from '../runtime/core.js';
import { displayln as print } from '../runtime/kernel.rkt.js';

const demoNamespace = Expander.make_namespace();
const KERNEL_IMPORT = CORE.Pair.makeList(
    CORE.PrimitiveSymbol.make('quote'),
    CORE.PrimitiveSymbol.make('#%kernel')
);

const STX_KERNEL_IMPORT = CORE.Pair.makeList(
    CORE.PrimitiveSymbol.make('for-syntax'),
    CORE.Pair.makeList(
        CORE.PrimitiveSymbol.make('quote'),
        CORE.PrimitiveSymbol.make('#%kernel')
    )
);

const SUM = CORE.Pair.makeList(CORE.PrimitiveSymbol.make('+'), 5, 5);

const LET_EX = CORE.Pair.makeList(
    CORE.PrimitiveSymbol.make('let'),
    CORE.Pair.makeList(CORE.Pair.makeList(CORE.PrimitiveSymbol.make('x'), 5)),
    CORE.PrimitiveSymbol.make('x')
);

const DEFINE_EX = CORE.Pair.makeList(
    CORE.PrimitiveSymbol.make('let'),
    CORE.Pair.makeList(),
    CORE.Pair.makeList(
        CORE.PrimitiveSymbol.make('define'),
        CORE.PrimitiveSymbol.make('x'),
        5
    ),
    CORE.Pair.makeList(
        CORE.PrimitiveSymbol.make('set!'),
        CORE.PrimitiveSymbol.make('x'),
        6
    ),
    CORE.PrimitiveSymbol.make('x')
);

Expander.namespace_attach_module(Expander.current_namespace(), KERNEL_IMPORT, demoNamespace);

Expander.namespace_require(KERNEL_IMPORT, demoNamespace);
Expander.namespace_require(STX_KERNEL_IMPORT, demoNamespace);

const expandExpr = (e) => {
    const stxObj = Expander.namespace_syntax_introduce(Expander.datum__gt_syntax(false, e), demoNamespace);
    return Expander.expand(stxObj, demoNamespace);
};

const compileExpr = (e) => {
    expandExpr(e);
    return Expander.compile(e, demoNamespace);
};

print(compileExpr(DEFINE_EX));
