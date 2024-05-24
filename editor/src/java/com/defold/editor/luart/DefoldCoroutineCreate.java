/*******************************************************************************
 * Copyright (c) 2007-2012 LuaJ. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/
package com.defold.editor.luart;


import clojure.lang.Var;
import clojure.lang.Volatile;
import org.luaj.vm2.*;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.VarArgFunction;

public class DefoldCoroutineCreate extends OneArgFunction {

    private final Globals globals;

    public DefoldCoroutineCreate(Globals globals) {
        this.globals = globals;
    }

    @Override
    public LuaValue call(LuaValue arg) {
        LuaFunction func = arg.checkfunction();
        Volatile vol = new Volatile(null);
        VarArgFunction threadBoundFunc = new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                Var.resetThreadBindingFrame(vol.deref());
                return func.invoke(args);
            }
        };
        return new LuaThread(globals, threadBoundFunc) {
            @Override
            public Varargs resume(Varargs args) {
                vol.reset(Var.cloneThreadBindingFrame());
                return super.resume(args);
            }
        };
    }
}
