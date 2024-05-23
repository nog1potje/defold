package com.defold.editor.luart;

import clojure.lang.IFn;
import org.luaj.vm2.lib.BaseLib;

import java.io.InputStream;

public class DefoldBaseLib extends BaseLib {
    private final IFn resourceFinder;

    public DefoldBaseLib(IFn resourceFinder) {
        this.resourceFinder = resourceFinder;
    }

    @Override
    public InputStream findResource(String filename) {
        return (InputStream) resourceFinder.invoke(filename);
    }
}
