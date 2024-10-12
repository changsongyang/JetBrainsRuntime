/*
 * Copyright 2024 JetBrains s.r.o.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package sun.awt.wl;

import sun.awt.im.InputMethodAdapter;
import sun.util.logging.PlatformLogger;

import java.awt.*;
import java.awt.im.spi.InputMethodContext;
import java.util.Locale;

public final class WLInputMethod extends InputMethodAdapter {

    // TODO: add logging everywhere
    private static final PlatformLogger log = PlatformLogger.getLogger("sun.awt.wl.WLInputMethod");


    /* sun.awt.im.InputMethodAdapter methods section */

    @Override
    protected Component getClientComponent() {
        return super.getClientComponent();
    }

    @Override
    protected boolean haveActiveClient() {
        return super.haveActiveClient();
    }

    @Override
    protected void setAWTFocussedComponent(Component component) {
        super.setAWTFocussedComponent(component);
    }

    @Override
    protected boolean supportsBelowTheSpot() {
        return super.supportsBelowTheSpot();
    }

    @Override
    protected void stopListening() {
        super.stopListening();
    }

    @Override
    public void notifyClientWindowChange(Rectangle location) {
        super.notifyClientWindowChange(location);
    }

    @Override
    public void reconvert() {
        super.reconvert();
    }

    @Override
    public void disableInputMethod() {
    }

    @Override
    public String getNativeInputMethodInfo() {
        return "";
    }


    /* java.awt.im.spi.InputMethod methods section */

    @Override
    public void setInputMethodContext(InputMethodContext context) {
    }

    @Override
    public boolean setLocale(Locale locale) {
        return false;
    }

    @Override
    public Locale getLocale() {
        return null;
    }

    @Override
    public void setCharacterSubsets(Character.Subset[] subsets) {
    }

    @Override
    public void setCompositionEnabled(boolean enable) {
    }

    @Override
    public boolean isCompositionEnabled() {
        return false;
    }

    @Override
    public void dispatchEvent(AWTEvent event) {
    }

    @Override
    public void activate() {
    }

    @Override
    public void deactivate(boolean isTemporary) {
    }

    @Override
    public void hideWindows() {
    }

    @Override
    public void removeNotify() {
    }

    @Override
    public void endComposition() {
    }

    @Override
    public void dispose() {
    }

    @Override
    public Object getControlObject() {
        return null;
    }


    /* Implementation details section */


    /* JNI downcalls section */

    /** Initializes all static JNI references ({@code jclass}, {@code jmethodID}, etc.) required by this class for functioning. */
    private static native void initIDs();

    /** @return pointer to the newly created native context associated with {@code this}. */
    private native long createNativeContext() throws AWTException;
    /** Disposes the native context created previously via {@link #createNativeContext()}. */
    private static native void disposeNativeContext(long contextPtr);

    /*private native void zwp_text_input_v3_destroy(long contextPtr);*/ // No use-cases for this currently
    private native void zwp_text_input_v3_enable(long contextPtr);
    private native void zwp_text_input_v3_disable(long contextPtr);
    /*private native void zwp_text_input_v3_set_surrounding_text();*/   // Not supported currently
    private native void zwp_text_input_v3_set_cursor_rectangle(long contextPtr, int surfaceLocalX, int surfaceLocalY, int width, int height);
    private native void zwp_text_input_v3_set_content_type(long contextPtr, int hint, int purpose);
    private native void zwp_text_input_v3_set_text_change_cause(long contextPtr, int changeCause);
    private native void zwp_text_input_v3_commit(long contextPtr);


    /* JNI upcalls section */

    /** Called in response to {@code zwp_text_input_v3::enter} events. */
    private void zwp_text_input_v3_onEnter(long enteredWlSurfacePtr) {
        assert EventQueue.isDispatchThread();
    }

    /** Called in response to {@code zwp_text_input_v3::leave} events. */
    private void zwp_text_input_v3_onLeave(long leftWlSurfacePtr) {
        assert EventQueue.isDispatchThread();
    }

    /** Called in response to {@code zwp_text_input_v3::preedit_string} events. */
    private void zwp_text_input_v3_onPreeditString(byte[] preeditStrUtf8, int cursorBeginUtf8Byte, int cursorEndUtf8Byte) {
        assert EventQueue.isDispatchThread();
    }

    /** Called in response to {@code zwp_text_input_v3::commit_string} events. */
    private void zwp_text_input_v3_onCommitString(byte[] commitStrUtf8) {
        assert EventQueue.isDispatchThread();
    }

    /** Called in response to {@code zwp_text_input_v3::delete_surrounding_text} events. */
    private void zwp_text_input_v3_onDeleteSurroundingText(long numberOfUtf8BytesBeforeToDelete, long numberOfUtf8BytesAfterToDelete) {
        assert EventQueue.isDispatchThread();
    }

    /** Called in response to {@code zwp_text_input_v3::done} events. */
    private void zwp_text_input_v3_onDone(long doneSerial) {
        assert EventQueue.isDispatchThread();
    }
}
