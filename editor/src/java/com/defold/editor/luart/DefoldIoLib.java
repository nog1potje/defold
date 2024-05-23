package com.defold.editor.luart;

import clojure.lang.IFn;
import org.luaj.vm2.lib.jse.JseIoLib;

import java.io.IOException;

public class DefoldIoLib extends JseIoLib {

    private final IFn resolveFileName;

    public DefoldIoLib(IFn resolveFileName) {
        this.resolveFileName = resolveFileName;
    }

    @Override
    protected File openFile(String filename, boolean readMode, boolean appendMode, boolean updateMode, boolean binaryMode) throws IOException {
        return super.openFile((String) resolveFileName.invoke(filename), readMode, appendMode, updateMode, binaryMode);
    }
}
