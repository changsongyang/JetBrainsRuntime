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

import java.awt.*;
import java.awt.font.TextAttribute;
import java.awt.im.InputMethodHighlight;
import java.awt.im.spi.InputMethod;
import java.awt.im.spi.InputMethodDescriptor;
import java.util.Locale;
import java.util.Map;

public final class WLInputMethodDescriptor implements InputMethodDescriptor {

    /** @see WLToolkit#mapInputMethodHighlight(InputMethodHighlight) */
    static Map<TextAttribute, ?> mapInputMethodHighlight(final InputMethodHighlight highlight) {
        // NB: The implementation is supposed to produce results exactly equal to XToolkit's implementation
        //     for better visual consistency.

        if (highlight == null)
            return null;

        switch (highlight.getState()) {
            case InputMethodHighlight.RAW_TEXT -> {
                if (highlight.isSelected())
                    return imHighlightMapSelectedRawText;
                else
                    return imHighlightMapUnselectedRawText;
            }

            case InputMethodHighlight.CONVERTED_TEXT -> {
                if (highlight.isSelected())
                    return imHighlightMapSelectedConvertedText;
                else
                    return imHighlightMapUnselectedConvertedText;
            }
        }

        return null;
    }


    public static boolean isAvailableOnPlatform() {
        return isAvailableOnPlatform;
    }

    public static WLInputMethodDescriptor getInstanceIfAvailableOnPlatform() {
        if (!isAvailableOnPlatform()) {
            return null;
        }
        return new WLInputMethodDescriptor();
    }


    @Override
    public Locale[] getAvailableLocales() throws AWTException {
        ensureIsAvailableOnPlatform();

        // This is how it's implemented in XToolkit.
        // For now I have no idea how to implement it better - it seems we need some API for
        //   obtaining all currently installed and enabled input sources
        //   (like Settings -> Keyboard -> Input Sources on GNOME), which then can be mapped to
        //   the corresponding locales. But, firstly, I know no suitable Linux API for this - the closest thing is
        //   to check all the layouts/groups in the XKB keymap we get from wl_keyboard::keymap events, but those
        //   keymaps don't contain information about input method sources, e.g. "Chinese (Intelligent Pinyin)".
        //   And secondly - I'm not even sure whether this is really the right approach.
        // So leaving as is at the moment.

        // TODO: research how to properly implement this (if it's wrong) along with {@link #hasDynamicLocaleList}

        assert toolkitStartupLocale != null;
        return new Locale[]{ toolkitStartupLocale };
    }

    @Override
    public boolean hasDynamicLocaleList() {
        // Since the return value of {@link #getAvailableLocales()} doesn't currently change over time,
        //   it doesn't make sense to return true here.
        return false;
    }

    @Override
    public String getInputMethodDisplayName(Locale inputLocale, Locale displayLanguage) {
        assert isAvailableOnPlatform();

        // We ignore the input locale.
        // When displaying for the default locale, rely on the localized AWT properties;
        //   for any other locale, fall back to English.
        String name = "System Input Methods";
        if (Locale.getDefault().equals(displayLanguage)) {
            name = Toolkit.getProperty("AWT.HostInputMethodDisplayName", name);
        }
        return name;
    }

    @Override
    public Image getInputMethodIcon(Locale inputLocale) {
        return null;
    }

    @Override
    public InputMethod createInputMethod() throws Exception {
        ensureIsAvailableOnPlatform();

        // We should avoid returning null from this method because the calling code isn't really ready to get null
        return new WLInputMethod();
    }


    /* Implementation details section */

    /**
     * The values are copied from XToolkit's implementation for better visual consistency with AWT on X11.
     *
     * @see #mapInputMethodHighlight(InputMethodHighlight)
     * @see sun.awt.X11InputMethodBase
     */
    private final static Map<TextAttribute, ?> imHighlightMapUnselectedRawText = Map.of(
        TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD
    );
    private final static Map<TextAttribute, ?> imHighlightMapUnselectedConvertedText = Map.of(
        TextAttribute.INPUT_METHOD_UNDERLINE, TextAttribute.UNDERLINE_LOW_ONE_PIXEL
    );
    private final static Map<TextAttribute, ?> imHighlightMapSelectedRawText = Map.of(
        TextAttribute.SWAP_COLORS, TextAttribute.SWAP_COLORS_ON
    );
    private final static Map<TextAttribute, ?> imHighlightMapSelectedConvertedText = Map.of(
        TextAttribute.SWAP_COLORS, TextAttribute.SWAP_COLORS_ON
    );

    /**
     * Only used as the return value for {@link #getAvailableLocales()}
     */
    private static Locale toolkitStartupLocale = null;

    private static final boolean isAvailableOnPlatform;

    static {
        isAvailableOnPlatform = checkIfAvailableOnPlatform();
    }

    private static void ensureIsAvailableOnPlatform() throws AWTException {
        if (!isAvailableOnPlatform()) {
            throw new AWTException("sun.awt.wl.WLInputMethod does not support this system");
        }
    }


    private WLInputMethodDescriptor() {
        assert isAvailableOnPlatform();

        if (toolkitStartupLocale == null) {
            toolkitStartupLocale = WLToolkit.getStartupLocale();
        }
    }


    /* JNI downcalls section */

    /**
     * This method checks if {@link WLInputMethod} can function on this system.
     * Basically, it means the Wayland compositor supports a minimal sufficient subset of the required protocols
     *   (currently the set only includes the "text-input-unstable-v3" protocol).
     *
     * @return true if {@link WLInputMethod} can function on this system, false otherwise
     * @see <a href="https://wayland.app/protocols/text-input-unstable-v3">text-input-unstable-v3</a>
     */
    private static native boolean checkIfAvailableOnPlatform();
}
