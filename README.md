# Clotrine

A ClojureScript template rendering engine. Currently dependent on node.js and
thus only meant for server-side code.

## Usage

For now, please take a look at the `test` directory for examples on how to use Clotrine.
All main features are covered by unit tests and instead of the `write-fn` used in the
tests, in actual application code you would pass in a function writing to a response
object, socket, or wherever you want to write your output to.

## License

Copyright © 2017 Jussi Nieminen

Distributed under the MIT License.
