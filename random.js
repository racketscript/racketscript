import * as Expander from './foo.js';
import * as CORE from '../runtime/core.js';

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

console.log(compileExpr(5));
