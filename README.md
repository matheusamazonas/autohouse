# Autohouse

macOS: [![Build Status](https://travis-ci.com/matheusamazonas/autohouse.svg?branch=master)](https://travis-ci.com/matheusamazonas/autohouse)

A house automation application developed as part of my [master's thesis](https://github.com/matheusamazonas/masterthesis) entitled "Developing Real Life, Task Oriented Applications for the Internet of Things". The application currenlty supports only Arduino boards.

The application is built using the functional language [Clean](https://clean.cs.ru.nl/Clean) and the TOP library [iTasks](https://clean.cs.ru.nl/ITasks).

Requirements
-----------
* Clean
* CLM
* CPM
* iTasks

All of the requirements are available [here](https://clean.cs.ru.nl/Download_Clean). Follow the instructions under "Clean and iTasks (Development)" and download `clean-bundle-itasks'.

Building
-----------
To build the application, run `make` on the repository directory.

Running
----------
To run the application, run the `AutoHouse` file generated by the build process. The web application can be accessed form any modern browser at `localhost:8080`.
