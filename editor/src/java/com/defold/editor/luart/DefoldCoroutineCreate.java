// Copyright 2020-2024 The Defold Foundation
// Copyright 2014-2020 King
// Copyright 2009-2014 Ragnar Svensson, Christian Murray
// Licensed under the Defold License version 1.0 (the "License"); you may not use
// this file except in compliance with the License.
//
// You may obtain a copy of the License, together with FAQs at
// https://www.defold.com/license
//
// Unless required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

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
