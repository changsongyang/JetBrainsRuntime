/*
 * Copyright 2025 JetBrains s.r.o.
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

package com.jetbrains.desktop.image;

import sun.awt.image.AcceleratedImageSurfaceManager;
import sun.awt.image.SurfaceManager;
import sun.java2d.SurfaceManagerFactory;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

public class AcceleratedImage extends Image {
    private final static ImageCapabilities caps = new ImageCapabilities(true);
    private final int width;
    private final int height;
    private final int transparency;
    private final AcceleratedImageSurfaceManager surfaceManager;

    public AcceleratedImage(GraphicsConfiguration gc, int width, int height, int transparency) {
        if (transparency != Transparency.OPAQUE && transparency != Transparency.TRANSLUCENT) {
            throw new IllegalArgumentException("Illegal transparency value: " + transparency);
        }

        this.width = width;
        this.height = height;
        this.transparency = transparency;
        surfaceManager = SurfaceManagerFactory.getInstance().createAcceleratedImageSurfaceManager(gc, this);
        SurfaceManager.setManager(this, surfaceManager);
    }

    @Override
    public int getWidth(ImageObserver observer) {
        return width;
    }

    @Override
    public int getHeight(ImageObserver observer) {
        return height;
    }

    public int getTransparency() {
        return transparency;
    }

    @Override
    public ImageProducer getSource() {
        BufferedImage bi = surfaceManager.getPrimarySurfaceData()
                .getDeviceConfiguration().createCompatibleImage(getWidth(null), getHeight(null));

        Graphics2D g = bi.createGraphics();
        g.setComposite(AlphaComposite.Src);
        g.drawImage(this, 0, 0, null);
        g.dispose();
        return bi.getSource();
    }

    @Override
    public Graphics getGraphics() {
        return null;
    }

    @Override
    public Object getProperty(String name, ImageObserver observer) {
        if (name == null) {
            throw new NullPointerException("null property name is not allowed");
        }
        return java.awt.Image.UndefinedProperty;
    }

    @Override
    public ImageCapabilities getCapabilities(GraphicsConfiguration gc) {
        return caps;
    }

    public long getNativeResource() {
        return surfaceManager.getNativeTexture();
    }
}
