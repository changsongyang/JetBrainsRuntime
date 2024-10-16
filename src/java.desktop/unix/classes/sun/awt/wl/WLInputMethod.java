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
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Locale;
import java.util.Objects;

public final class WLInputMethod extends InputMethodAdapter {

    // TODO: add logging everywhere
    private static final PlatformLogger log = PlatformLogger.getLogger("sun.awt.wl.WLInputMethod");


    public WLInputMethod() throws AWTException {
        wlInitializeContext();
    }

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
        wlDisposeContext();
    }

    @Override
    public Object getControlObject() {
        return null;
    }


    /* java.lang.Object methods section (overriding some of its methods) */

    @SuppressWarnings("removal")
    @Override
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }


    /* Implementation details section */

    // Since WLToolkit dispatches (almost) all native Wayland events on EDT, not on its thread,
    //   there's no need for this class to think about multithreading issues - all of its parts may only be executed
    //   on EDT.
    // If WLToolkit dispatched native Wayland events on its thread {@link sun.awt.wl.WLToolkit#isToolkitThread},
    //   this class would require the following modifications:
    //     - Guarding access to the fields with some synchronization primitives
    //     - Taking into account that zwp_text_input_v3_on* callbacks may even be called when the constructor doesn't
    //       even return yet (in the current implementation)
    //     - Reworking the implementation of {@link #disposeNativeContext(long)} so that it prevents
    //       use-after-free access errors to the destroyed native context from the native handlers of
    //       zwp_text_input_v3 native events.

    static {
        initIDs();
    }

    /**
     * The interface serves just as a namespace for all the types, constants
     * and helper (static) methods required for work with the "text-input-unstable-v3" protocol.
     * It has no declared non-static methods or subclasses/implementors.
     */
    private interface ZwpTextInputV3 {
        /** Reason for the change of surrounding text or cursor position */
        enum ChangeCause {
            INPUT_METHOD(0), // input method caused the change
            OTHER       (1); // something else than the input method caused the change

            public final int intValue;
            ChangeCause(int intValue) {
                this.intValue = intValue;
            }
        }

        /** Content hint is a bitmask to allow to modify the behavior of the text input */
        enum ContentHint {
            NONE               (0x0),   // no special behavior
            COMPLETION         (0x1),   // suggest word completions
            SPELLCHECK         (0x2),   // suggest word corrections
            AUTO_CAPITALIZATION(0x4),   // switch to uppercase letters at the start of a sentence
            LOWERCASE          (0x8),   // prefer lowercase letters
            UPPERCASE          (0x10),  // prefer uppercase letters
            TITLECASE          (0x20),  // prefer casing for titles and headings (can be language dependent)
            HIDDEN_TEXT        (0x40),  // characters should be hidden
            SENSITIVE_DATA     (0x80),  // typed text should not be stored
            LATIN              (0x100), // just Latin characters should be entered
            MULTILINE          (0x200); // the text input is multiline

            public final int intMask;
            ContentHint(int intMask) {
                this.intMask = intMask;
            }
        }

        /**
         * The content purpose allows to specify the primary purpose of a text input.
         * This allows an input method to show special purpose input panels with extra characters or to disallow some characters.
         */
        enum ContentPurpose {
            NORMAL  (0),  // default input, allowing all characters
            ALPHA   (1),  // allow only alphabetic characters
            DIGITS  (2),  // allow only digits
            NUMBER  (3),  // input a number (including decimal separator and sign)
            PHONE   (4),  // input a phone number
            URL     (5),  // input an URL
            EMAIL   (6),  // input an email address
            NAME    (7),  // input a name of a person
            PASSWORD(8),  // input a password (combine with sensitive_data hint)
            PIN     (9),  // input is a numeric password (combine with sensitive_data hint)
            DATE    (10), // input a date
            TIME    (11), // input a time
            DATETIME(12), // input a date and time
            TERMINAL(13); // input for a terminal

            public final int intValue;
            ContentPurpose(int intValue) {
                this.intValue = intValue;
            }
        }


        // zwp_text_input_v3::set_text_change_cause
        ChangeCause       INITIAL_VALUE_TEXT_CHANGE_CAUSE = ChangeCause.INPUT_METHOD;
        // zwp_text_input_v3::set_content_type.hint
        int               INITIAL_VALUE_CONTENT_HINT      = ContentHint.NONE.intMask;
        // zwp_text_input_v3::set_content_type.purpose
        ContentPurpose    INITIAL_VALUE_CONTENT_PURPOSE   = ContentPurpose.NORMAL;
        // zwp_text_input_v3::set_cursor_rectangle
        /**
         * The initial values describing a cursor rectangle are empty.
         * That means the text input does not support describing the cursor area.
         * If the empty values get applied, subsequent attempts to change them may have no effect.
         */
        Rectangle         INITIAL_VALUE_CURSOR_RECTANGLE  = null;
        // zwp_text_input_v3::preedit_string
        JavaPreeditString INITIAL_VALUE_PREEDIT_STRING    = new JavaPreeditString("", 0, 0);
        // zwp_text_input_v3::commit_string
        JavaCommitString  INITIAL_VALUE_COMMIT_STRING     = new JavaCommitString("");


        // Below are a few classes designed to maintain the state of an input context (represented by an instance of
        // {@code zwp_text_input_v3}).
        //
        // The state itself is stored in InputContext.
        // The classes OutgoingChanges and OutgoingBeingCommittedChanges represent a set of changes the client (us)
        //   sends to the compositor. OutgoingChanges accumulates changes until they're committed via zwp_text_input_v3_commit,
        //   and OutgoingBeingCommittedChanges keeps changes after they get committed and until they actually get applied
        //   by the compositor. After that, the applied changes get reflected in the InputContext.
        // The class IncomingChanges represents a set of changes that the client receives from the compositor through
        //   the events of zwp_text_input_v3.
        //
        // All the classes are designed as data structures with no business logic; the latter should be
        //   encapsulated by WLInputMethod class itself. However, the write-access to the fields is
        //   still only provided via methods (instead of having the fields public) just to ensure the validity of
        //   the changes and to better express their purposes.

        /**
         * This class encapsulates the entire state of an input context represented by an instance of {@code zwp_text_input_v3}.
         *
         * @see StateOfEnabled
         */
        final class InputContext {
            /** {@link #createNativeContext()} / {@link #disposeNativeContext(long)} */
            public final long nativeContextPtr;


            public InputContext(long nativeContextPtr) {
                assert(nativeContextPtr != 0);

                this.nativeContextPtr = nativeContextPtr;
            }

            /**
             * This class represents the extended state of an {@code InputContext} that only exists when the context
             *   is enabled.
             *
             * @param textChangeCause the property set via a {@code zwp_text_input_v3::set_text_change_cause} request. Must not be {@code null}.
             * @param contentHint the property set via a {@code zwp_text_input_v3::set_content_type} request.
             * @param contentPurpose the property set via a {@code zwp_text_input_v3::set_content_type} request. Must not be {@code null}.
             * @param cursorRectangle the property set via a {@code zwp_text_input_v3::set_cursor_rectangle} request.
             */
            public record StateOfEnabled(
                // zwp_text_input_v3::set_text_change_cause
                ChangeCause textChangeCause,
                // zwp_text_input_v3::set_content_type.hint
                int contentHint,
                // zwp_text_input_v3::set_content_type.purpose
                ContentPurpose contentPurpose,
                // zwp_text_input_v3::set_cursor_rectangle
                Rectangle cursorRectangle
            ) {
                public StateOfEnabled {
                    Objects.requireNonNull(textChangeCause, "textChangeCause");
                    Objects.requireNonNull(contentPurpose, "contentPurpose");
                }
            }


            @Override
            public String toString() {
                final StringBuilder sb = new StringBuilder(512);
                sb.append("InputContext@").append(System.identityHashCode(this));
                sb.append('{');
                sb.append("nativeContextPtr=").append(nativeContextPtr);
                sb.append(", currentWlSurfacePtr=").append(currentWlSurfacePtr);
                sb.append(", commitCounter=").append(commitCounter);
                sb.append(", latestDoneSerial=").append(latestDoneSerial);
                sb.append(", stateOfEnabled=").append(stateOfEnabled);
                sb.append(", latestAppliedPreeditString=").append(latestAppliedPreeditString);
                sb.append(", latestAppliedCommitString=").append(latestAppliedCommitString);
                sb.append('}');
                return sb.toString();
            }


            // zwp_text_input_v3::enter.surface / zwp_text_input_v3::leave.surface
            private long currentWlSurfacePtr = 0;
            // zwp_text_input_v3::commit
            /**
             * How many times changes to this context have been committed (through {@code zwp_text_input_v3::commit}).
             * Essentially, it means the most actual version of the context's state.
             */
            private long commitCounter = 0;
            // zwp_text_input_v3::done.serial
            /**
             * The {@code serial} parameter of the latest {@code zwp_text_input_v3::done} event received.
             * Essentially, it means the latest version of the context's state known/confirmed by the compositor.
             */
            private long latestDoneSerial = 0;
            /** {@code null} if the InputContext is disabled. */
            private StateOfEnabled stateOfEnabled = null;
            /**
             * The latest preedit string applied as a result of the latest {@code zwp_text_input_v3::done} event received.
             * Must never be {@code null} ; if a {@code zwp_text_input_v3::done} event wasn't preceded by a
             * {@code zwp_text_input_v3::preedit_string} event, the field should be set to {@link #INITIAL_VALUE_PREEDIT_STRING}.
             */
            private JavaPreeditString latestAppliedPreeditString = INITIAL_VALUE_PREEDIT_STRING;
            /**
             * The latest preedit string applied as a result of the latest {@code zwp_text_input_v3::done} event received.
             * Must never be {@code null} ; if a {@code zwp_text_input_v3::done} event wasn't preceded by a
             * {@code zwp_text_input_v3::commit_string} event, the field should be set to {@link #INITIAL_VALUE_COMMIT_STRING}.
             */
            private JavaCommitString latestAppliedCommitString = INITIAL_VALUE_COMMIT_STRING;
        }


        /**
         * This class is intended to accumulate changes for an {@link InputContext} until
         *   they're sent via the set of methods
         *   {@link #zwp_text_input_v3_enable(long)}, {@link #zwp_text_input_v3_disable(long)}, {@code zwp_text_input_v3_set_*}
         *   and commited via {@link #zwp_text_input_v3_commit(long)}.
         * <p>
         * The reason of having to accumulate changes instead of applying them as soon as they appear is the following
         * part of the {@code zpw_text_input_v3::done(serial)} event specification:
         * {@code
         * When the client receives a done event with a serial different than the number of past commit requests,
         * it must proceed with evaluating and applying the changes as normal, except it should not change the
         * current state of the zwp_text_input_v3 object. All pending state requests [...]
         * on the zwp_text_input_v3 object should be sent and committed after receiving a
         * zwp_text_input_v3.done event with a matching serial.
         * }
         *<p>
         * All the properties this class includes are nullable where {@code null} means absent of this property change.
         * In other words, if a property is null, the corresponding {@code zwp_text_input_v3_set_*} shouldn't be
         * called when processing this instance of OutgoingChanges.
         * <p>
         * The modifier methods return {@code this} for method chaining.
         */
        final class OutgoingChanges
        {
            // zwp_text_input_v3::enable / zwp_text_input_v3::disable
            private Boolean newEnabled = null;

            // zwp_text_input_v3::set_text_change_cause
            private ChangeCause newTextChangeCause = null;

            // zwp_text_input_v3::set_content_type
            private Integer newContentTypeHint = null;
            private ContentPurpose newContentTypePurpose = null;

            // zwp_text_input_v3::set_cursor_rectangle
            private Rectangle newCursorRectangle = null;


            @Override
            public String toString() {
                final StringBuilder sb = new StringBuilder(256);
                sb.append("OutgoingChanges@").append(System.identityHashCode(this));
                sb.append('[');
                sb.append("newEnabled=").append(newEnabled);
                sb.append(", newTextChangeCause=").append(newTextChangeCause);
                sb.append(", newContentTypeHint=").append(newContentTypeHint);
                sb.append(", newContentTypePurpose=").append(newContentTypePurpose);
                sb.append(", newCursorRectangle=").append(newCursorRectangle);
                sb.append(']');
                return sb.toString();
            }


            public OutgoingChanges setEnabledState(Boolean newEnabled) {
                this.newEnabled = newEnabled;
                return this;
            }

            public Boolean getEnabledState() { return newEnabled; }


            public OutgoingChanges setTextChangeCause(ChangeCause newTextChangeCause) {
                this.newTextChangeCause = newTextChangeCause;
                return this;
            }

            public ChangeCause getTextChangeCause() { return newTextChangeCause; }


            /**
             * Both parameters have to be {@code null} or not null simultaneously.
             *
             * @throws NullPointerException if one of the parameters is {@code null} while the other one is not.
             */
            public OutgoingChanges setContentType(Integer newContentTypeHint, ContentPurpose newContentTypePurpose) {
                if (newContentTypeHint == null && newContentTypePurpose == null) {
                    this.newContentTypeHint = null;
                    this.newContentTypePurpose = null;
                } else {
                    final var contentHintAllMask =
                        ContentHint.NONE.intMask |
                        ContentHint.COMPLETION.intMask |
                        ContentHint.SPELLCHECK.intMask |
                        ContentHint.AUTO_CAPITALIZATION.intMask |
                        ContentHint.LOWERCASE.intMask |
                        ContentHint.UPPERCASE.intMask |
                        ContentHint.TITLECASE.intMask |
                        ContentHint.HIDDEN_TEXT.intMask |
                        ContentHint.SENSITIVE_DATA.intMask |
                        ContentHint.LATIN.intMask |
                        ContentHint.MULTILINE.intMask;

                    if ( (Objects.requireNonNull(newContentTypeHint, "newContentTypeHint") & ~contentHintAllMask) != 0 ) {
                        throw new IllegalArgumentException(String.format("newContentTypeHint=%d has invalid bits set", newContentTypeHint));
                    }

                    this.newContentTypeHint = newContentTypeHint;
                    this.newContentTypePurpose = Objects.requireNonNull(newContentTypePurpose, "newContentTypePurpose");
                }
                return this;
            }

            public Integer getContentTypeHint() { return newContentTypeHint; }
            public ContentPurpose getContentTypePurpose() { return newContentTypePurpose; }


            public OutgoingChanges setCursorRectangle(Rectangle newCursorRectangle) {
                this.newCursorRectangle = newCursorRectangle;
                return this;
            }

            public Rectangle getCursorRectangle() { return newCursorRectangle; }
        }

        /**
         *
         * @param changeSet changes that have been sent and committed to the compositor,
         *                  but not yet confirmed by it (via a {@code zwp_text_input_v3::done} event).
         *                  Must not be {@code null}.
         * @param commitCounter the number of times a {@code zwp_text_input_v3::commit} request has been issued to
         *                      the corresponding InputContext.
         *
         * @see OutgoingChanges
         */
        record OutgoingBeingCommittedChanges(OutgoingChanges changeSet, long commitCounter) {
            public OutgoingBeingCommittedChanges {
                Objects.requireNonNull(changeSet, "changeSet");
            }
        }


        /**
         * This class accumulates changes received as
         * {@code zwp_text_input_v3::preedit_string}, {@code zwp_text_input_v3::commit_string} events until
         * a {@code zwp_text_input_v3::done} event is received.
         */
        final class IncomingChanges
        {
            public IncomingChanges updatePreeditString(byte[] newPreeditStringUtf8, int newPreeditStringCursorBeginUtf8Byte, int newPreeditStringCursorEndUtf8Byte) {
                this.doUpdatePreeditString = true;
                this.newPreeditStringUtf8 = newPreeditStringUtf8;
                this.newPreeditStringCursorBeginUtf8Byte = newPreeditStringCursorBeginUtf8Byte;
                this.newPreeditStringCursorEndUtf8Byte = newPreeditStringCursorEndUtf8Byte;
                this.cachedResultPreeditString = null;

                return this;
            }

            /**
             * @return {@code null} if there are no changes in the preedit string
             *                      (i.e. {@link #updatePreeditString(byte[], int, int)} hasn't been called);
             *         an instance of JavaPreeditString otherwise.
             * @see ZwpTextInputV3.JavaPreeditString
             */
            public ZwpTextInputV3.JavaPreeditString getPreeditString() {
                if (cachedResultPreeditString != null) {
                    return cachedResultPreeditString;
                }

                cachedResultPreeditString = doUpdatePreeditString
                    ? JavaPreeditString.fromWaylandPreeditString(newPreeditStringUtf8, newPreeditStringCursorBeginUtf8Byte, newPreeditStringCursorEndUtf8Byte)
                    : null;

                return cachedResultPreeditString;
            }


            public IncomingChanges updateCommitString(byte[] newCommitStringUtf8) {
                this.doUpdateCommitString = true;
                this.newCommitStringUtf8 = newCommitStringUtf8;
                this.cachedResultCommitString = null;

                return this;
            }

            /**
             * @return {@code null} if there are no changes in the commit string
             *                     (i.e. {@link #updateCommitString(byte[])}  hasn't been called);
             *         an instance of JavaCommitString otherwise.
             * @see JavaCommitString
             */
            public JavaCommitString getCommitString() {
                if (cachedResultCommitString != null) {
                    return cachedResultCommitString;
                }

                cachedResultCommitString = doUpdateCommitString
                        ? JavaCommitString.fromWaylandCommitString(newCommitStringUtf8)
                        : null;

                return cachedResultCommitString;
            }


            @Override
            public boolean equals(Object o) {
                if (o == null || getClass() != o.getClass()) return false;
                IncomingChanges that = (IncomingChanges) o;
                return doUpdatePreeditString == that.doUpdatePreeditString &&
                       newPreeditStringCursorBeginUtf8Byte == that.newPreeditStringCursorBeginUtf8Byte &&
                       newPreeditStringCursorEndUtf8Byte == that.newPreeditStringCursorEndUtf8Byte &&
                       doUpdateCommitString == that.doUpdateCommitString &&
                       Objects.deepEquals(newPreeditStringUtf8, that.newPreeditStringUtf8) &&
                       Objects.deepEquals(newCommitStringUtf8, that.newCommitStringUtf8);
            }

            @Override
            public int hashCode() {
                return Objects.hash(
                    doUpdatePreeditString,
                    Arrays.hashCode(newPreeditStringUtf8),
                    newPreeditStringCursorBeginUtf8Byte,
                    newPreeditStringCursorEndUtf8Byte,
                    doUpdateCommitString,
                    Arrays.hashCode(newCommitStringUtf8)
                );
            }


            // zwp_text_input_v3::preedit_string
            private boolean doUpdatePreeditString = false;
            private byte[] newPreeditStringUtf8 = null;
            private int newPreeditStringCursorBeginUtf8Byte = 0;
            private int newPreeditStringCursorEndUtf8Byte = 0;
            private JavaPreeditString cachedResultPreeditString = null;

            // zwp_text_input_v3::commit_string
            private boolean doUpdateCommitString = false;
            private byte[] newCommitStringUtf8 = null;
            private JavaCommitString cachedResultCommitString = null;
        }


        // Utility/helper classes and methods

        static int getLengthOfUtf8BytesWithoutTrailingNULs(final byte[] utf8Bytes) {
            int lastNonNulIndex = (utf8Bytes == null) ? -1 : utf8Bytes.length - 1;
            for (; lastNonNulIndex >= 0; --lastNonNulIndex) {
                if (utf8Bytes[lastNonNulIndex] != 0) {
                    break;
                }
            }

            return (lastNonNulIndex < 0) ? 0 : lastNonNulIndex + 1;
        }

        static String utf8BytesToJavaString(final byte[] utf8Bytes) {
            if (utf8Bytes == null) {
                return "";
            }

            return utf8BytesToJavaString(
                utf8Bytes,
                0,
                // Java's UTF-8 -> UTF-16 conversion doesn't like trailing NUL codepoints, so let's trim them
                getLengthOfUtf8BytesWithoutTrailingNULs(utf8Bytes)
            );
        }

        static String utf8BytesToJavaString(final byte[] utf8Bytes, final int offset, final int length) {
            return utf8Bytes == null ? "" : new String(utf8Bytes, offset, length, StandardCharsets.UTF_8);
        }


        /**
         * This class represents the result of a conversion of a UTF-8 preedit string received in a
         * {@code zwp_text_input_v3::preedit_string} event to a Java UTF-16 string.
         * If {@link #cursorBeginCodeUnit} and/or {@link #cursorEndCodeUnit} point at UTF-16 surrogate pairs,
         *   they're guaranteed to point at the very beginning of them as long as {@link #fromWaylandPreeditString} is
         *   used to perform the conversion.
         * <p>
         * {@link #fromWaylandPreeditString} never returns {@code null}.
         * <p>
         * See the specification of {@code zwp_text_input_v3::preedit_string} event for more info about
         * cursor_begin, cursor_end values.
         *
         * @param text The preedit text string. Mustn't be {@code null} (use an empty string instead).
         * @param cursorBeginCodeUnit UTF-16 equivalent of {@code preedit_string.cursor_begin}.
         * @param cursorEndCodeUnit UTF-16 equivalent of {@code preedit_string.cursor_end}.
         *                          It's not explicitly stated in the protocol specification, but it seems to be a valid
         *                          situation when cursor_end < cursor_begin, which means
         *                          the highlight extends to the right from the caret
         *                          (e.g., when the text gets selected with Shift + Left Arrow).
         */
        record JavaPreeditString(String text, int cursorBeginCodeUnit, int cursorEndCodeUnit) {
            public JavaPreeditString {
                Objects.requireNonNull(text, "text");
            }

            public static final JavaPreeditString EMPTY = new JavaPreeditString("", 0, 0);

            public static JavaPreeditString fromWaylandPreeditString(
                final byte[] utf8Bytes,
                final int cursorBeginUtf8Byte,
                final int cursorEndUtf8Byte
            ) {
                // Java's UTF-8 -> UTF-16 conversion doesn't like trailing NUL codepoints, so let's trim them
                final int utf8BytesWithoutNulLength = getLengthOfUtf8BytesWithoutTrailingNULs(utf8Bytes);

                // cursorBeginUtf8Byte, cursorEndUtf8Byte normalized relatively to the valid values range.
                final int fixedCursorBeginUtf8Byte;
                final int fixedCursorEndUtf8Byte;
                if (cursorBeginUtf8Byte < 0 || cursorEndUtf8Byte < 0) {
                    fixedCursorBeginUtf8Byte = fixedCursorEndUtf8Byte = -1;
                } else {
                    // 0 <= cursorBeginUtf8Byte <= fixedCursorBeginUtf8Byte <= utf8BytesWithoutNulLength
                    fixedCursorBeginUtf8Byte = Math.min(cursorBeginUtf8Byte, utf8BytesWithoutNulLength);
                    // 0 <= cursorEndUtf8Byte <= fixedCursorEndUtf8Byte <= utf8BytesWithoutNulLength
                    fixedCursorEndUtf8Byte = Math.min(cursorEndUtf8Byte, utf8BytesWithoutNulLength);
                }

                final var resultText = utf8BytesToJavaString(utf8Bytes, 0, utf8BytesWithoutNulLength);

                if (fixedCursorBeginUtf8Byte < 0 || fixedCursorEndUtf8Byte < 0) {
                    return new JavaPreeditString(resultText, -1, -1);
                }

                if (resultText == null) {
                    assert(fixedCursorBeginUtf8Byte == 0);
                    assert(fixedCursorEndUtf8Byte == 0);

                    return JavaPreeditString.EMPTY;
                }

                final String javaPrefixBeforeCursorBegin = (fixedCursorBeginUtf8Byte == 0)
                                                           ? ""
                                                           : utf8BytesToJavaString(utf8Bytes, 0, fixedCursorBeginUtf8Byte);

                final String javaPrefixBeforeCursorEnd = (fixedCursorEndUtf8Byte == 0)
                                                         ? ""
                                                         : utf8BytesToJavaString(utf8Bytes, 0, fixedCursorEndUtf8Byte);

                return new JavaPreeditString(
                    resultText,
                    javaPrefixBeforeCursorBegin.length(),
                    javaPrefixBeforeCursorEnd.length()
                );
            }
        }

        record JavaCommitString(String text) {
            public JavaCommitString {
                Objects.requireNonNull(text, "text");
            }

            public static final JavaCommitString EMPTY = new JavaCommitString("");

            /** Never returns {@code null}. */
            public static JavaCommitString fromWaylandCommitString(byte[] utf8Bytes) {
                return new JavaCommitString(utf8BytesToJavaString(utf8Bytes));
            }
        }
    }


    /* Wayland-side state section */

    // The fields in this section are prefixed with "wl" and aren't supposed to be modified by
    //   non-Wayland-related methods (whose names are prefixed with "wl" or "zwp_text_input_v3_"),
    //   though can be read by them.

    /** The reference must only be (directly) modified in {@link #wlInitializeContext()} and {@link #wlDisposeContext()}. */
    private ZwpTextInputV3.InputContext wlInputContextState = null;


    /* Wayland-side methods section */

    // The methods in this section implement the core logic of working with the zwp_text_input_v3 protocol.

    private void wlInitializeContext() throws AWTException {
        assert(wlInputContextState == null);

        long nativeCtxPtr = 0;

        try {
            nativeCtxPtr = createNativeContext();
            if (nativeCtxPtr == 0) {
                throw new AWTException("nativeCtxPtr == 0");
            }

            wlInputContextState = new ZwpTextInputV3.InputContext(nativeCtxPtr);
        } catch (Throwable err) {
            if (nativeCtxPtr != 0) {
                disposeNativeContext(nativeCtxPtr);
                nativeCtxPtr = 0;
            }

            throw err;
        }
    }

    private void wlDisposeContext() {
        final var ctxToDispose = this.wlInputContextState;

        wlInputContextState = null;

        if (ctxToDispose != null && ctxToDispose.nativeContextPtr != 0) {
            disposeNativeContext(ctxToDispose.nativeContextPtr);
        }
    }


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
