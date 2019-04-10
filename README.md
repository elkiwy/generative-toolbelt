# generative-toolbelt

Generative-toolbelt is a rich set of Clojure functions to ease out the process
of drawing and sketching with Quil. These are the function that I
personally use when I'm creating my artworks.

## Modules

The library is made of different sub-modules to individually manage
shapes, to let you require only the part necessary to your project.

Each module comes with the functions to create, manage, and draw the
structures representing that particular shape, plus some handy utility
functions here and there.

Currently the modules implemented are:

* **Point.clj**: Point, Vector, and Line structures. 

* **Circle.clj**: Circle and Ellipse structures.

* **Path.clj**: Paths structures with conversion and smoothing functions.

* **Triangle.clj**: Triangles structures with recursive triangle splitting
  and area triangulation functions.

* **Utils.clj**: Canvas operations, draw options setters, random function wrappers, and
  sequences operations.

* **Color.clj**: A draft of a Color structure to easily manage colors,
  still needs implementation.

The library is still in development and will be updated and expanded
as I find the need of some new functions inside my drawings.

If you have any request feel free to open an issue here on GitHub and
I'll try to implement that into the library!


## Usage

You can use the source as external files to import into your project,
or you can simply add this:

    [elkiwy/generative-toolbelt "0.1.1-SNAPSHOT"]

to the dependencies in your `project.clj` file.


## Documentation

You can check the full automatically generated documentation with
Codox at https://elkiwy.github.io/generative-toolbelt/


## Example

Here are some example of the images that I generated through this
library. If you want to see these images at high resolution with other
images like these you can check out my website at https://elkiwyart.com


<img align="left" src="https://raw.githubusercontent.com/elkiwy/generative-toolbelt/master/examples/Color%20Mountains%20739953.png" width="400">

<img align="left" src="https://raw.githubusercontent.com/elkiwy/generative-toolbelt/master/examples/Deep%20Stars%20861183.png" width="400">

<img align="left" src="https://raw.githubusercontent.com/elkiwy/generative-toolbelt/master/examples/lines1.png" width="400">

<img align="left" src="https://raw.githubusercontent.com/elkiwy/generative-toolbelt/master/examples/nuggets2.png" width="400">



## License

Copyright © 2019 Stefano Bertoli
Distributed under the MIT License
