# elm-image-slider [![Build Status](https://travis-ci.org/larribas/elm-image-slider.svg?branch=master)](https://travis-ci.org/larribas/elm-image-slider)
An image slider / slideshow / carousel component for Elm


## [Try it out](https://larribas.github.io/elm-image-slider/)

## How to use it

Install the package:

```
elm-package install larribas/elm-image-slider
```

The `demo/Demo.elm` file is a good example of how this component can be integrated into an Elm application. It demonstrates several recommended use patterns:

* Managing the messages to open and close the Slider from the outer context
* Using the abstract Model and Config to pass in a list of custom image types
* Leveraging the `caption` function to display clickable links
* Using the default, responsive sylesheet at `styles/image-slider.css` (warning: it's based on flexbox, so you might want to tweak it a bit if you have to code like it's 2014)
* Assigning custom elements classes from the component's default styles so that they integrate nicely with the component's visualization



## Contribute

Any contributions or feedback are welcome!
