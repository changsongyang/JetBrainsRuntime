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
        this.awtNativeImIsExplicitlyDisabled = true;
        wlDisableContextNow();

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
        this.awtNativeImIsExplicitlyDisabled = true;
        wlDisableContextNow();
    }

    @Override
    public String getNativeInputMethodInfo() {
        return "";
    }


    /* java.awt.im.spi.InputMethod methods section */

    @Override
    public void setInputMethodContext(InputMethodContext context) {
        this.awtImContext = context;
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
        this.awtActivationStatus = AWTActivationStatus.ACTIVATED;
        this.awtNativeImIsExplicitlyDisabled = false;

        // It may be wrong to invoke this only if awtActivationStatus was DEACTIVATED.
        // E.g. if there was a call chain [activate -> disableInputMethod -> activate].
        // So let's enable the context here regardless of the previous value of awtActivationStatus.
        if (wlContextHasToBeEnabled() && wlContextCanBeEnabledNow()) {
            wlEnableContextNow();
        }
    }

    @Override
    public void deactivate(boolean isTemporary) {
        final boolean wasActive = (this.awtActivationStatus == AWTActivationStatus.ACTIVATED);
        this.awtActivationStatus = isTemporary ? AWTActivationStatus.DEACTIVATED_TEMPORARILY : AWTActivationStatus.DEACTIVATED;

        if (wasActive) {
            wlDisableContextNow();
        }
    }

    @Override
    public void hideWindows() {
    }

    @Override
    public void removeNotify() {
        // "The method is only called when the input method is inactive."
        assert(this.awtActivationStatus != AWTActivationStatus.ACTIVATED);

        wlDisableContextNow();
    }

    @Override
    public void endComposition() {
    }

    @Override
    public void dispose() {
        awtActivationStatus = AWTActivationStatus.DEACTIVATED;
        awtNativeImIsExplicitlyDisabled = false;
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


            /** @return 0 if the input context hasn't entered a surface yet. Otherwise, the native pointer to the surface. */
            public long getCurrentWlSurfacePtr() {
                return currentWlSurfacePtr;
            }


            /**
             * Notifies the InputContext that a set of changes has been sent and committed to the compositor
             *   via a {@code zwp_text_input_v3::commit} request. The InputContext reacts by incrementing its commit counter.
             *
             * @param changes represents the set of changes that have been sent and followed by a 'commit' request.
             *                Must not be {@code null} (but can be empty, which means only the 'commit' request has been issued).
             *
             * @return a new instance of {@link OutgoingBeingCommittedChanges} consisting of
             *         the passed changes and the new value of the commit counter.
             *
             * @throws NullPointerException if {@code changes} is {@code null}.
             *
             * @see OutgoingChanges
             */
            public OutgoingBeingCommittedChanges syncWithCommittedOutgoingChanges(final OutgoingChanges changes) {
                Objects.requireNonNull(changes, "changes");

                // zwp_text_input_v3::done natively uses uint32_t for the serial,
                //   so it can't get greater than 0xFFFFFFFF.
                this.commitCounter = (this.commitCounter + 1) % 0x100000000L;

                return new OutgoingBeingCommittedChanges(changes, this.commitCounter);
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

            public StateOfEnabled getCurrentStateOfEnabled() {
                return stateOfEnabled;
            }

            public boolean isEnabled() {
                return getCurrentStateOfEnabled() != null;
            }

            /**
             * NB: if you want to call setEnabledState(null), consider using {@link #wlHandleContextGotDisabled()}.
             *
             * @param newState {@code null} to mark the InputContext as disabled,
             *                 otherwise the InputContext will be marked as enabled and having the state as
             *                 specified in the parameter.
             */
            public void setEnabledState(StateOfEnabled newState) {
                this.stateOfEnabled = newState;
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


            public OutgoingChanges appendChangesFrom(OutgoingChanges src) {
                if (src == null) return this;

                if (getTextChangeCause() == null) {
                    setTextChangeCause(src.getTextChangeCause());
                }
                if (getContentTypeHint() == null) {
                    setContentType(src.getContentTypeHint(), src.getContentTypePurpose());
                }
                if (getCursorRectangle() == null) {
                    setCursorRectangle(src.getCursorRectangle());
                }

                return this;
            }
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


    /* AWT-side state section */

    // The fields in this section are prefixed with "awt" and aren't supposed to be modified by
    //   Wayland-related methods (whose names are prefixed with "wl" or "zwp_text_input_v3_"),
    //   though can be read by them.

    private enum AWTActivationStatus {
        ACTIVATED,               // #activate()
        DEACTIVATED,             // #deactivate(false)
        DEACTIVATED_TEMPORARILY  // #deactivate(true)
    }

    /** {@link #activate()} / {@link #deactivate(boolean)} */
    private AWTActivationStatus awtActivationStatus = AWTActivationStatus.DEACTIVATED;
    /** {@link #stopListening()}, {@link #disableInputMethod()} / {@link #activate()} */
    private boolean awtNativeImIsExplicitlyDisabled = false;
    /** {@link #setInputMethodContext(InputMethodContext)} */
    private InputMethodContext awtImContext = null;


    /* AWT-side methods section */

    private static void awtFillWlContentTypeOf(Component component, ZwpTextInputV3.OutgoingChanges out) {
        assert(component != null);
        assert(out != null);

        assert(EventQueue.isDispatchThread());

        // TODO: there's no dedicated AWT/Swing API for that, but we can make a few guesses, e.g.
        //       (component instanceof JPasswordField) ? ZwpTextInputV3.ContentPurpose.PASSWORD
        out.setContentType(ZwpTextInputV3.ContentHint.NONE.intMask, ZwpTextInputV3.ContentPurpose.NORMAL);
    }

    private static void awtFillWlCursorRectangleOf(Component component, ZwpTextInputV3.OutgoingChanges out) {
        assert(component != null);
        assert(out != null);

        assert(EventQueue.isDispatchThread());

        // TODO: real implementation
        out.setCursorRectangle(new Rectangle(0, 0, 1, 1));
    }


    /* Wayland-side state section */

    // The fields in this section are prefixed with "wl" and aren't supposed to be modified by
    //   non-Wayland-related methods (whose names are prefixed with "wl" or "zwp_text_input_v3_"),
    //   though can be read by them.

    /** The reference must only be (directly) modified in {@link #wlInitializeContext()} and {@link #wlDisposeContext()}. */
    private ZwpTextInputV3.InputContext wlInputContextState = null;
    /** Accumulates changes to be sent. {@code null} means there are no changes to send yet. */
    private ZwpTextInputV3.OutgoingChanges wlPendingChanges = null;
    /** Changes that have been committed but not yet applied by the compositor. {@code null} means there are no such changes at the moment. */
    private ZwpTextInputV3.OutgoingBeingCommittedChanges wlBeingCommittedChanges = null;


    /* Wayland-side methods section */

    // The methods in this section implement the core logic of working with the zwp_text_input_v3 protocol.

    private void wlInitializeContext() throws AWTException {
        assert(wlInputContextState == null);
        assert(wlPendingChanges == null);
        assert(wlBeingCommittedChanges == null);

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
        wlPendingChanges = null;
        wlBeingCommittedChanges = null;

        if (ctxToDispose != null && ctxToDispose.nativeContextPtr != 0) {
            disposeNativeContext(ctxToDispose.nativeContextPtr);
        }
    }


    /**
     * This method determines whether any of the pending state changes can be sent and committed.
     */
    private boolean wlCanSendChangesNow() {
        return wlInputContextState != null &&
               wlInputContextState.nativeContextPtr != 0 &&
               wlBeingCommittedChanges == null;
    }

    /**
     * Transforms the pending set of changes into a series of corresponding zwp_text_input_v3 requests
     *   followed by a {@code zwp_text_input_v3::commit} request.
     */
    private void wlSendPendingChangesNow() {
        assert(wlCanSendChangesNow());

        final ZwpTextInputV3.OutgoingChanges changesToSend = wlPendingChanges;
        wlPendingChanges = null;

        if (wlInputContextState.getCurrentWlSurfacePtr() == 0) {
            // "After leave event, compositor must ignore requests from any text input instances until next enter event."
            // Thus, it doesn't make sense to send any requests, let's drop the change set.

            if (log.isLoggable(PlatformLogger.Level.FINE)) {
                log.fine("wlSendPendingChangesNow: wlInputContextState.getCurrentWlSurfacePtr()=0. Dropping the change set {0}", changesToSend);
            }

            return;
        }

        // TODO: make sure the current AWT component belongs to the surface wlInputContextState.getCurrentWlSurfacePtr()

        if (log.isLoggable(PlatformLogger.Level.FINER)) {
            log.finer("wlSendPendingChangesNow: sending the change set: {0}.", changesToSend);
        }

        if (changesToSend != null) {
            if (Boolean.TRUE.equals(changesToSend.getEnabledState())) {
                if (this.awtActivationStatus != AWTActivationStatus.ACTIVATED) {
                    throw new IllegalStateException("Attempt to enable an input context while the owning WLInputMethod is not active. WLInputMethod.awtActivationStatus=" + this.awtActivationStatus);
                }
                if (this.awtNativeImIsExplicitlyDisabled) {
                    throw new IllegalStateException("Attempt to enable an input context while it must stay disabled.");
                }
                zwp_text_input_v3_enable(wlInputContextState.nativeContextPtr);
            }

            if (changesToSend.getTextChangeCause() != null) {
                zwp_text_input_v3_set_text_change_cause(wlInputContextState.nativeContextPtr,
                                                        changesToSend.getTextChangeCause().intValue);
            }

            if (changesToSend.getContentTypeHint() != null && changesToSend.getContentTypePurpose() != null) {
                zwp_text_input_v3_set_content_type(wlInputContextState.nativeContextPtr,
                                                   changesToSend.getContentTypeHint(),
                                                   changesToSend.getContentTypePurpose().intValue);
            }

            if (changesToSend.getCursorRectangle() != null) {
                zwp_text_input_v3_set_cursor_rectangle(wlInputContextState.nativeContextPtr,
                                                       changesToSend.getCursorRectangle().x,
                                                       changesToSend.getCursorRectangle().y,
                                                       changesToSend.getCursorRectangle().width,
                                                       changesToSend.getCursorRectangle().height);
            }

            if (Boolean.FALSE.equals(changesToSend.getEnabledState())) {
                zwp_text_input_v3_disable(wlInputContextState.nativeContextPtr);
            }
        }

        zwp_text_input_v3_commit(wlInputContextState.nativeContextPtr);

        wlBeingCommittedChanges = wlInputContextState.syncWithCommittedOutgoingChanges(changesToSend);
    }

    /**
     * Schedules a new set of changes for sending (but doesn't send them).
     *
     * @see #wlSendPendingChangesNow()
     * @see #wlCanSendChangesNow()
     */
    private void wlScheduleContextNewChanges(final ZwpTextInputV3.OutgoingChanges newOutgoingChanges) {
        if (newOutgoingChanges == null) {
            return;
        }

        this.wlPendingChanges = newOutgoingChanges.appendChangesFrom(this.wlPendingChanges);
    }


    private boolean wlContextHasToBeEnabled() {
        return awtActivationStatus == AWTActivationStatus.ACTIVATED &&
               !awtNativeImIsExplicitlyDisabled &&
               !wlInputContextState.isEnabled();
    }

    private boolean wlContextCanBeEnabledNow() {
        return awtActivationStatus == AWTActivationStatus.ACTIVATED &&
               !awtNativeImIsExplicitlyDisabled &&
               wlInputContextState.getCurrentWlSurfacePtr() != 0;
    }

    private void wlEnableContextNow() {
        // The method's implementation is based on the following assumptions:
        //   1. Enabling an input context from the zwp_text_input_v3 protocol's point of view can be done at any moment,
        //      even when there are committed changes, which the compositor hasn't applied yet,
        //      i.e. even when (this.wlBeingCommittedChanges != null).
        //      The protocol specification doesn't seem to contradict this assumption, and otherwise it would significantly
        //      complicate the machinery of scheduling changes in general and enabling, disabling routines in particular.
        //   2. Committed 'enable' request comes into effect immediately and doesn't hinder any following requests to be sent
        //      right after, even though a corresponding 'done' event hasn't been received.
        //      This assumption has been made because the protocol doesn't specify whether compositors should
        //      respond to committed 'enable' requests with a 'done' event, and, in practice,
        //      Mutter responds with a 'done' event while KWin - doesn't.
        //      The corresponding ticket: https://gitlab.freedesktop.org/wayland/wayland-protocols/-/issues/250.

        if (awtActivationStatus != AWTActivationStatus.ACTIVATED) {
            throw new IllegalStateException("Attempt to enable an input context while the owning WLInputMethod is not active. WLInputMethod.awtActivationStatus=" + this.awtActivationStatus);
        }
        if (awtNativeImIsExplicitlyDisabled) {
            throw new IllegalStateException("Attempt to enable an input context while it must stay disabled.");
        }
        if (wlInputContextState.getCurrentWlSurfacePtr() == 0) {
            throw new IllegalStateException("Attempt to enable an input context which hasn't entered any surface");
        }

        assert(wlContextCanBeEnabledNow());

        // This way we guarantee the context won't accidentally get disabled because such a change has been scheduled earlier.
        // Anyway we consider any previously scheduled changes outdated because an 'enable' request is supposed to
        //   reset the state of the input context.
        wlPendingChanges = null;

        if (wlInputContextState.isEnabled()) {
            if (wlBeingCommittedChanges == null) {
                // We can skip sending a new 'enable' request only if there's currently nothing being committed.
                // This way we can guarantee the context won't accidentally get disabled afterward or
                //   be keeping outdated state.

                return;
            }
        }

        final var changeSet =
            new ZwpTextInputV3.OutgoingChanges()
                .setEnabledState(true)
                // Just to signal the compositor we're supporting set_text_change_cause API
                .setTextChangeCause(ZwpTextInputV3.INITIAL_VALUE_TEXT_CHANGE_CAUSE);
        awtFillWlContentTypeOf(getClientComponent(), changeSet);
        awtFillWlCursorRectangleOf(getClientComponent(), changeSet);

        wlScheduleContextNewChanges(changeSet);
        assert(wlPendingChanges != null);

        // Pretending there are no committed, but not applied yet changes, so that wlCanSendChangesNow() is true.
        // We can do that because the assumption #1 and because any previously committed changes get lost when a
        // 'enable' request is committed:
        //   "This request resets all state associated with previous enable, disable,
        //    set_surrounding_text, set_text_change_cause, set_content_type, and set_cursor_rectangle requests [...]"
        wlBeingCommittedChanges = null;

        assert(wlCanSendChangesNow());
        wlSendPendingChangesNow();

        // See the assumption #2 above.
        wlSyncWithAppliedOutgoingChanges();
    }

    private void wlDisableContextNow() {
        // The method's implementation is based on the following assumptions:
        //   1. Disabling an input context from the zwp_text_input_v3 protocol's point of view can be done at any moment,
        //      even when there are committed changes, which the compositor hasn't applied yet,
        //      i.e. even when (this.wlBeingCommittedChanges != null).
        //      The protocol specification doesn't seem to contradict this assumption, and otherwise it would significantly
        //      complicate the machinery of scheduling changes in general and enabling, disabling routines in particular.
        //   2. Committed 'disable' request comes into effect immediately and doesn't hinder any following requests to be sent
        //      right after, even though a corresponding 'done' event hasn't been received.
        //      This assumption has been made because the protocol doesn't specify whether compositors should
        //      respond to committed 'disable' requests with a 'done' event, and, in practice, neither Mutter nor KWin do that.
        //      The corresponding ticket: https://gitlab.freedesktop.org/wayland/wayland-protocols/-/issues/250.

        // This way we guarantee the context won't accidentally get enabled because such a change has been scheduled earlier.
        // Anyway we consider any previously scheduled changes outdated because a 'disable' request is supposed to
        //   reset the state of the input context.
        wlPendingChanges = null;

        if (wlInputContextState.getCurrentWlSurfacePtr() == 0) {
            // In this case it doesn't make sense to send any requests:
            //   "After leave event, compositor must ignore requests from any text input instances until next enter event."
            // The context is supposed to have been automatically implicitly disabled.

            // Any being committed changes are meaningless, so we can safely "forget" about them.
            wlBeingCommittedChanges = null;

            if (wlInputContextState.isEnabled()) {
                if (log.isLoggable(PlatformLogger.Level.WARNING)) {
                    log.warning("The input context is marked as enabled although it's not focused on any surface. Explicitly marking it as disabled. wlInputContextState={0}", wlInputContextState);
                }
                wlHandleContextGotDisabled();
            }

            return;
        }

        if (!wlInputContextState.isEnabled()) {
            if (wlBeingCommittedChanges == null) {
                // We can skip sending a new 'disable' request only if there's currently nothing being committed.
                // This way we can guarantee the context won't accidentally get enabled afterward as a result of
                //   those changes' processing.

                return;
            }
        }

        assert(wlInputContextState.getCurrentWlSurfacePtr() != 0);

        wlScheduleContextNewChanges(new ZwpTextInputV3.OutgoingChanges().setEnabledState(false));
        assert(wlPendingChanges != null);

        // Pretending there are no committed, but not applied yet changes, so that wlCanSendChangesNow() is true.
        // We can do that because the assumption #1 and because any previously committed changes get lost when a
        // 'disable' request is committed:
        //   "After an enter event or disable request all state information is invalidated and needs to be resent by the client."
        wlBeingCommittedChanges = null;

        assert(wlCanSendChangesNow());
        wlSendPendingChangesNow();

        // See the assumption #2 above.
        wlSyncWithAppliedOutgoingChanges();
    }

    private void wlHandleContextGotDisabled() {
        wlInputContextState.setEnabledState(null);

        try {
            // TODO: delete or commit the current preedit text in the current client component
        } catch (Exception err) {
            if (log.isLoggable(PlatformLogger.Level.WARNING)) {
                log.warning("wlHandleContextGotDisabled", err);
            }
        }
    }


    private void wlSyncWithAppliedOutgoingChanges() {
        final var changesToSyncWith = wlBeingCommittedChanges;
        wlBeingCommittedChanges = null;

        if (changesToSyncWith == null) {
            return;
        }

        if (Boolean.FALSE.equals(changesToSyncWith.changeSet.getEnabledState())) {
            wlHandleContextGotDisabled();
        } else if (Boolean.TRUE.equals(changesToSyncWith.changeSet.getEnabledState())) {
            // 'enable' request
            // "resets all state associated with previous enable, disable,
            //  set_surrounding_text, set_text_change_cause, set_content_type, and set_cursor_rectangle requests [...]"
            // So here we just convert the changeSet to a new StateOfEnabled and apply it.

            wlInputContextState.setEnabledState(new ZwpTextInputV3.InputContext.StateOfEnabled(
                Objects.requireNonNullElse(changesToSyncWith.changeSet.getTextChangeCause(), ZwpTextInputV3.INITIAL_VALUE_TEXT_CHANGE_CAUSE),
                Objects.requireNonNullElse(changesToSyncWith.changeSet.getContentTypeHint(), ZwpTextInputV3.INITIAL_VALUE_CONTENT_HINT),
                Objects.requireNonNullElse(changesToSyncWith.changeSet.getContentTypePurpose(), ZwpTextInputV3.INITIAL_VALUE_CONTENT_PURPOSE),
                changesToSyncWith.changeSet.getCursorRectangle() == null ? ZwpTextInputV3.INITIAL_VALUE_CURSOR_RECTANGLE : changesToSyncWith.changeSet.getCursorRectangle()
            ));
        } else if (wlInputContextState.isEnabled()) {
            // The changes are only supposed to update the current StateOfEnabled

            final var currentStateOfEnabled = wlInputContextState.getCurrentStateOfEnabled();

            wlInputContextState.setEnabledState(new ZwpTextInputV3.InputContext.StateOfEnabled(
                // "The value set with this request [...] must be applied and reset to initial at the next zwp_text_input_v3.commit request."
                Objects.requireNonNullElse(changesToSyncWith.changeSet.getTextChangeCause(), ZwpTextInputV3.INITIAL_VALUE_TEXT_CHANGE_CAUSE),

                // "Values set with this request [...] will get applied on the next zwp_text_input_v3.commit request.
                //  Subsequent attempts to update them may have no effect."
                currentStateOfEnabled.contentHint(),
                currentStateOfEnabled.contentPurpose(),

                // "Values set with this request [...] will get applied on the next zwp_text_input_v3.commit request,
                //  and stay valid until the next committed enable or disable request."
                changesToSyncWith.changeSet.getCursorRectangle() == null ? currentStateOfEnabled.cursorRectangle() : changesToSyncWith.changeSet.getCursorRectangle()
            ));
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

        if (wlContextHasToBeEnabled() && wlContextCanBeEnabledNow()) {
            wlEnableContextNow();
        }
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

        if (wlContextHasToBeEnabled() && wlContextCanBeEnabledNow()) {
            wlEnableContextNow();
        }
        if (wlPendingChanges != null && wlInputContextState.getCurrentWlSurfacePtr() != 0 && wlCanSendChangesNow()) {
            wlSendPendingChangesNow();
        }
    }
}
