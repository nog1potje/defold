package com.defold.editor.luart;

import clojure.lang.IFn;
import clojure.lang.RT;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.VarArgFunction;

public class DefoldVarArgFn extends VarArgFunction {

    private final IFn fn;

    public DefoldVarArgFn(IFn fn) {
        this.fn = fn;
    }

    @Override
    public Varargs invoke(Varargs args) {
        int n = args.narg();
        Object[] array = new Object[n];
        for (int i = 0; i < n; i++) {
            array[i] = args.arg(i + 1);
        }
        return (Varargs) fn.applyTo(RT.seq(array));
    }
}
