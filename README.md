raytracer
=========

![Generated Image](https://github.com/torus/raytracer/raw/master/untitled.jpg)

- raytracer.cpp: Original C++ implementation from
  http://www.scratchapixel.com/lessons/3d-basic-lessons/lesson-1-writing-a-simple-raytracer/
- raytracer.scm: Scheme implementation which runs on Gauche

This script generates a file named `untitled.ppm`. To see the image, you may need to convert it to other format such as JPEG.

    $ gosh raytracer.scm
    $ ppmtojpeg -quality 100 untitled.ppm > untitled.jpg
