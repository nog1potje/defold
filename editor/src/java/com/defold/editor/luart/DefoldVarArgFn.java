package com.defold.editor.luart;

import clojure.lang.IFn;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.VarArgFunction;

public class DefoldVarArgFn extends VarArgFunction {

    private final IFn fn;

    public DefoldVarArgFn(IFn fn) {
        this.fn = fn;
    }

    @Override
    public Varargs invoke(Varargs args) {
        return (Varargs) fn.invoke(args);
    }
}
