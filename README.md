# Lambda Calculus Repl

An intuitive repl for lambda calculus written in Elm.

![Repl Responsive Design Demo](./media/repl_responsive_demo.png)

Try repl [here](https://alienkevin.github.io/lambda-calculus-untyped/)!

# Features

* Cell-based repl with easy navigation
* Seamless multi-line cell editing
* Instant evaluations and problem feedbacks
* Auto save repl
* Support several color themes
* Tailored experiences for all devices

# Themes

## Light

![repl light theme demo](./media/repl_light_theme_demo.png)

## Solarized Light

![repl solarized light theme demo](./media/repl_solarized_light_theme_demo.png)

## Dark

![repl dark theme demo](./media/repl_dark_theme_demo.png)

# Development
1. Install [elm-live](https://github.com/wking-io/elm-live).

2. At the project root directory, start elm-live server
```
./start.sh
```
> Note: You may need to run `chmod +x ./start.sh` to enable execution permission.

# Deployment
At the project root directory, run `build.sh` to create an optimized and minimized build:
```
./build.sh
```
> Note: You may need to run `chmod +x ./build.sh` to enable execution permission.

# License
MIT

# Change Log

## Release 0.7.0

* Align repl to top of window
* Fix icon z-index hierarchy
* Color error messages tomato red

## Release 0.6.0

* Support expr without name in repl
* Fix indentation parsing
* Properly handle key up and down in multi-row cells
* Allow comments before and after def or expr in repl
* Support 3 color themes

## Release v0.5.0

* Add close active cell button
* Better visual hierarchy in popups
* Add link to source code in help popup

## Release v0.4.0

* Fix substitution issues by switching to De Bruijn Indices
* Support full evaluation
* Protect against infinite recursion like the merge function (\x. x x) (\x. x x)
* Clean up parentheses wrapping
* Return evaluated defs in original order

## Release v0.3.0

* Save model automatically to LocalStorage
* Add settings popup for evaluation strategy
* Add tooltips for buttons

## Release v0.2.0

* Evaluate all cells when editing one cell
* Track cell index in checker for problems like duplicated definition
* Enforce end of def and expr when parseDef and parseExpr
* Replace line prompt with cell index
* Place add cell button at the active cell

# Release v0.1.0

* Basic repl support