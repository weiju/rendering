# Rosetta Raytracer - Ray tracing in multiple programming languages

![Example image](example.png)

## Description

This project contains implementations of the ray tracing algorithms
and techniques in different languages to explore and compare them under
different aspects such as design, performance and parallelism.

Currently these languages are

  * Python 3
  * Scala
  * Racket

I picked those 3 for now for no other reasons than that I like them:

  * Python for fast prototyping
  * Scala for a speedy, tight implementation
  * Racket for meditating

## Dependencies

### Python version

The Python version depends on

  * NumPy for linear algebra
  * PyGame for graphics output
  * Pillow for images

### Scala version

The Scala version uses play-json as its JSON library since the Scala standard library does not support JSON anymore.
Version 2.3.10 is used because higher versions depend on Java 8

### Racket version

This version was developed with Racket 6.5, since Racket's base package is fairly comprehensive,
there are no other dependencies, simply load it in DrRacket and run, the result is a
PNG file

## Notes about Scene format

Scenes are specified in JSON, the structure was designed so to support a clean parser structure
if possible.

## References

  1. "An Introduction to Raytracing", Andrew S. Glassner et al., 1989, Academic Press
  2. [Steve Harvey, C++-Raytracer](https://steveharveynz.wordpress.com/category/programming/c-raytracer/)
  3. [Scratchapixel, series on ray tracing](http://www.scratchapixel.com/)
  4. [Wikipedia, Ray tracing entry](https://en.wikipedia.org/wiki/Ray_tracing_(graphics))
  5. [ForTheScience.org, series about ray tracing](http://forthescience.org/blog/2011/09/05/a-raytracer-in-python-part-1-basic-functionality/)
