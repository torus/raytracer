raytracer
=========

![Generated Image](https://github.com/torus/raytracer/raw/master/untitled.jpg)

Requirements
------------

- Gauche
- Gauche-gl (only for vector calculation)
- Netpbm

Description
-----------

- raytracer.cpp: Original C++ implementation from
  http://www.scratchapixel.com/lessons/3d-basic-lessons/lesson-1-writing-a-simple-raytracer/
- raytracer.scm: Scheme implementation which runs on Gauche

Invoking
--------

This script generates image data in PPM format. To see the image, you may need to convert it to other format such as JPEG.

    $ gosh raytracer.scm > result.ppm
    $ ppmtojpeg -quality 100 result.ppm > result.jpg
