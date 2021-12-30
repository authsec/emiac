# EMacs In A Container (emiac)

This projects aims to package a version of emacs suitable for scientific writing with org-mode.

As the intention is to render the documents using a LaTeX environment, the container runtime environment sits on top of authsec/sphinx that already includes a full text-live installation.

Additionally there is support for other publishing frameworks directly in the container. So everything you need to painstakingly install otherwise is already packaged in the container. If something is missing the base (or this) container can easily add the functionality and therefore abstract the writing environment from the runtime OS.

## Shell aliases

In order to start emiac easily it is recommended to set up an alias, or function, that will expand to the full command running `emiacs`.

### MacOS

The default shell on MacOS is `zsh`, so copy and paste the function into your `~/.zshrc`. If you don't have that file, simply create it and start a new Terminal.

**NOTE:** This will create a new container and therefore be a new process. If you want a new window in the same container instance, simply use `C-x 5 2` or similar to create a new frame.

The function assumes the container runtime environment has been started with:

**NOTE:** If you redo this, you need to `colima delete` first, as it seems to read from a cache somewhere, and the mount point is not applied. (This will also **delete all your downloaded images**, so don't do this if you do not have internet access to download/build the images again!!)

```
colima start --mount $HOME/research:w --mount $HOME/coding:w
```

and the function also requires the [latex-styles](https://github.com/authsec/latex-styles) project to be present under `coding/github/latex-styles`, as this will be mapped into the container too, to provide a customized style (if you work on the style, you want to set it `rw` instead of `ro`).

```
emiac() {
    docker run -d -it --rm -e DISPLAY=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'):0 -v ~/research:/home/emiac/research:rw -v ~/coding/github/latex-styles:/home/emiac/texmf/tex/latex/commonstuff:ro authsec/emiac 
}
```

## Runtime Environment

This setup assumes you do have a container runtime environment up and running.

The general consensus is most likely [Docker](https://www.docker.com/) here, as it works on all major operating systems.

With docker desktop starting to cost money for organizational use I guess people will move away from Docker Desktop towards [podman](https://podman.io/) or other alternatives.

### MacOS Monterey

I'm on a M1 Mac and had problems with the podman runtime being unable to properly mount directories into the container. [Colima](https://github.com/abiosoft/colima) however seems to work fine.

The quick and dirty is (you need [Homebrew](https://brew.sh/)):

```
#> brew install --cask xquartz
#> brew install colima
#> colima start --mount $HOME/research:w --mount $HOME/emiac_config_folder:w
#> docker run -it --rm -e DISPLAY=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'):0 -v ~/research:/home/emiac/research:rw -v ~/emiac_config_folder/setup:/home/emiac/.emiac/setup:rw authsec/emiac
```

**NOTE:** In future version of colima you may however NOT NEED to specify the potential mountpoints upfront.