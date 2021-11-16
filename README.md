# Interactive Test Runner Web

![badge](https://github.com/jproyo/test-runner/actions/workflows/test-runner-build.yaml/badge.svg)

WebApp Test Runner

## Design 
The design has been done according to the following characteristics:

**Architectural Layers**: There are two layers in the project *Client* (UI) and *Server*. The *Client* layer has been designed following the principle of a SPA (Single Page Application). Regarding the *Server*, it dispatches the main `index.html` content and listens to requests in a REST API.

### Client

The code of the UI part is in the `client` folder.
It has been built using [elm-lang](https://elm-lang.org/) language, despite being the first time I use it. In general terms, it has been easy for me to adapt and create an application reading the basic documentation. Regarding the lack of knowledge and practice with the language, there should be for sure a lot of things to be improved on this layer.

### Server

The code of the Server Side part is in the `server` folder.
The backend side, which serves the main `index.html` page and the REST API To submit the test, running them and querying them has been built in [Haskell](https://www.haskell.org/). 

Server side has been built using:

- `servant`: As an HTTP Server for the REST API and serving static content
- `polysemy`: As an Effectful system to have a better design that helps for extensions and improvements in the future, as well as testing.
- `inline-js`: As a JS runner for the Tests that are submitted from the UI.
- Other helper libraries like `colog` for logging, and `relude` for not using implicit `Prelude`.

#### Code Organization:

- `server/app`: Contains the `Main.hs` and the principal configuration file.
- `server/src`: Contains the Server Side as a library code.
- `server/src/App`: Contains the file that allows exposing the REST API in Servant and all the Data Types and combinators that helps in that task.
- `server/src/Data`: Data Types common to the REST API and the library
- `server/src/Effects`: Effects and Interpreters
- `server/src/JS`: Derivation of types for `inline-js`
- `server/src/Runner`: Program that uses the effects and composes them to fulfill the requirements.
- `server/test`: Test of Programs

## Building and Running the Program

The program provides a `Makefile` to help the user to build and start the project easily.

> NOTE: By default the server will start in port 7001. If you have any issue with that port it can be changed in the configuration file `server/app/conf.yaml` or providing the environment variable `TEST_RUNNER_PORT`.

### Prerequisites

- Stack 2.7.1+
- GHC 8.10.4+
- npm 7.21.1+
- node v16.9.1
- elm 0.19.1-3

### Building the program

```shell
> make
```

### Cleaning the program

> NOTE: Only in case you need to delete the binaries for some reason.

```shell
> make clean
```

### Running testing

```shell
> make test
```

### Running Application

```shell
> make start
```

Or changing the port 

```shell
> TEST_RUNNER_PORT=5005 make start
```

