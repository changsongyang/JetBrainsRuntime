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


    /* JNI upcalls section */
}
