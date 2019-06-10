## Docker Purescript Workflow

### Instalation

All you need to install for this setup is docker.

Once you have installed docker get the latest version of the [purescript docker image](https://hub.docker.com/r/fretlink/purescript-docker).

```sh
docker pull fretlink/purescript-docker:0.12.0
```

Then create a folder in which you would like your project.

```sh
mkdir myproject
``` 

Find your purescript docker image name using `docker images`.
And then run it with a volume:

```sh
docker run --rm -itv ~/../myproject/:/home/pureuser/src b09608732ec8 bash
```

Now, in this container we will initialize, complie our purescript into js, test our purescript, and run our repl while editing from our base machine.

### Basic Purescript Setup

To initialize: `pulp --psc-package init`
To build: `pulp --psc-package build`
For repl: `pulp --psc-package repl`
To install dependency: `psc-package install <package name>`

To build into one js file: `pulp --psc-package build --to dist/app.js`
