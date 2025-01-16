package com.jetbrains.desktop;

import com.jetbrains.exported.JBRApi;
import sun.awt.image.SunVolatileImage;
import sun.java2d.Surface;
import sun.java2d.SurfaceData;

import java.awt.image.VolatileImage;

public class NativeRasterLoader {
    /**
     * Loads native image raster into VolatileImage
     *
     * @param pRaster native pointer image raster with 8-bit RGBA color components packed into integer pixels.
     * Note: The color data in this image is considered to be premultiplied with alpha.
     * @param width width of image in pixels
     * @param height height of image in pixels
     * @param pRects native pointer to array of "dirty" rects, each rect is a sequence of four 32-bit integers: x, y, width, heigth
     * Note: can be null (then whole image used)
     * @param rectsCount count of "dirty" rects (if 0 then whole image used)
     */
    @JBRApi.Provides("NativeRasterLoader")
    static void loadNativeRaster(VolatileImage vi, long pRaster, int width, int height, long pRects, int rectsCount) {
        if (!(vi instanceof SunVolatileImage)) {
            System.err.printf("Unsupported type of VolatileImage: %s\n", vi);
            return;
        }

        SunVolatileImage svi = (SunVolatileImage)vi;
        sun.java2d.NativeRasterLoader.loadNativeRaster(svi.getDestSurface(), pRaster, width, height, pRects, rectsCount);
    }

    public static boolean loadTexture(VolatileImage vi, long pTexture) {
        if (!(vi instanceof SunVolatileImage)) {
            System.err.printf("Unsupported type of VolatileImage: %s\n", vi);
            return false;
        }

        Surface surface = ((SunVolatileImage) vi).getDestSurface();
        if (surface instanceof SurfaceData) {
            return ((SurfaceData)surface).loadTexture(pTexture);
        }
        return false;
    }
}