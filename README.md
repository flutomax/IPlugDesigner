# IPlug Designer
### Designer program for iPlug2 C++ audio plug-in framework 

[![Download Latest](https://img.shields.io/badge/download-latest-green.svg)](https://github.com/flutomax/IPlugDesigner/releases/)
[![Donate](https://img.shields.io/badge/donate-paypal-blue.svg)](https://paypal.me/flutomax)

The designer was specially created for more productive development of VST plugins based on the iPlug2 framework.
You can visually place controls in the design layout and get C++ code at the output, which you can later insert into your working plug-in code. Working with the program resembles in concept some visual IDEs, for example Lazarus, Delphi and the like.

![ScreenShot](/screenshots/iplug_designer_main_form.png)

The program assumes work with raster and vector controls, as well as based on SVG graphics. You can also save bitmaps, SVG and vector styles in the project file, which will allow you not to worry about the safety of resources. You can also create additional graphic elements that will be part of the design, but will not be converted to the original code. There is support for fontaudio fonts.
The program provides for the export of the design as a PNG image with an alpha channel, and it is also possible to copy the generated C++ code.

![ScreenShot](/screenshots/iplug_designer_code_form.png)

To compile, you need Lazarus version 2.0.12 with FPC 3.2.0 or higher. And also the latest version of the [Graphics32](http://www.graphics32.org/) framework.
Perhaps the project will be compiled with earlier versions of Lazarus and FPS, but this has not been tested.

Download the version compiled for Windows x64 and all the files necessary for the program to work in one archive [here](https://github.com/flutomax/IPlugDesigner/releases/).
An example of a design is attached.

