#include <jni.h>

#include <windows.h>
#include <GL/gl.h>

//typedef void (APIENTRYP PFNGLBINDTEXTUREPROC)(GLenum, GLuint);
//typedef void (APIENTRYP PFNGLGENTEXTURESPROC)(GLsizei, GLuint*);
//typedef void (APIENTRYP PFNGLTEXPARAMETERIPROC)(GLenum, GLenum, GLint);
//typedef void (APIENTRYP PFNGLTEXIMAGE2DPROC)(GLenum, GLint, GLint, GLsizei, GLsizei, GLint, GLenum, GLenum, const void*);

typedef void (APIENTRY *glBindTextureType)(GLenum target, GLuint texture);
typedef void (APIENTRY *glGenTexturesType)(GLsizei n, GLuint *textures);
typedef void (APIENTRY *glTexParameteriType)(GLenum target, GLenum pname, GLint param);
typedef void (APIENTRY *glTexImage2DType)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels);

//#pragma comment(lib, "opengl32.lib")


static glBindTextureType pglBindTexture;
static glGenTexturesType pglGenTextures;
static glTexParameteriType pglTexParameteri;
static glTexImage2DType pglTexImage2D;


JNIEXPORT void JNICALL Java_SharedTexturesTest_initNative
        (JNIEnv *env, jclass clazz, jlong pSharedContext, jint pixelFormat) {
    pglGenTextures = (glGenTexturesType)wglGetProcAddress("glGenTextures");
    pglBindTexture = (glBindTextureType)wglGetProcAddress("glBindTexture");
    pglTexParameteri = (glTexParameteriType)wglGetProcAddress("glTexParameteri");
    pglTexImage2D = (glTexImage2DType)wglGetProcAddress("glTexImage2D");

    if (pglGenTextures == 0 || pglBindTexture == 0 || pglTexParameteri == 0 || pglTexImage2D) {
        (*env)->ThrowNew(env, (*env)->FindClass(env, "java/lang/RuntimeException"),
            "Failed to load required OpenGL functions");
    }
}

JNIEXPORT jlong JNICALL Java_SharedTexturesTest_createTexture
        (JNIEnv *env, jclass clazz, jbyteArray byteArray, jint width, jint height, jint type) {
    if (type != 2) {
        return 0;
    }

    if (!pglGenTextures || !pglBindTexture || !pglTexParameteri || !pglTexImage2D) {
        return 0;
    }

    jsize length = (*env)->GetArrayLength(env, byteArray);
    jbyte *pixels = (*env)->GetByteArrayElements(env, byteArray, NULL);

    GLuint texId = 0;
    pglGenTextures(1, &texId);
    pglBindTexture(GL_TEXTURE_2D, texId);

    pglTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    pglTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    pglTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    pglTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    #ifndef GL_BGRA_EXT
    #define GL_BGRA_EXT 0x80E1
    #endif

    pglTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0,
                  GL_BGRA_EXT, GL_UNSIGNED_BYTE, (const void*)pixels);

    (*env)->ReleaseByteArrayElements(env, byteArray, pixels, JNI_ABORT);

    return (jlong)texId;
}

JNIEXPORT void JNICALL Java_SharedTexturesTest_disposeTexture
        (JNIEnv *env, jclass clazz, jlong pTexture, jint type) {

}
