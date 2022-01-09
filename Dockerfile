FROM ubuntu:focal AS build

ARG EMACS_VERSION="27.2"
ENV EMACS_VERSION=$EMACS_VERSION

WORKDIR /tmp

ARG DEBIAN_FRONTEND=noninteractive
# Install build dependencies (can also be used to build with gtk3 instead of the [here preferred] lucid toolkit)
RUN apt update && \
    apt -y install \
        curl \
        checkinstall \
        git \
        texinfo \
        install-info \
        build-essential \
        libgtk-3-dev \
        libtiff5-dev \
        libgif-dev \
        libjpeg-dev \
        libpng-dev \
        libxpm-dev \
        libncurses-dev \
        libwebkit2gtk-4.0-dev \
        libgnutls28-dev \
        autoconf \
        libxft-dev \
        libxaw7-dev \
        librsvg2-dev
# Download emacs 
RUN curl "https://ftp.gnu.org/pub/gnu/emacs/emacs-${EMACS_VERSION}.tar.gz" | tar xz && \
    mv emacs* emacs

WORKDIR /tmp/emacs

# Create emacs installer and make sure to use x-toolkit lucid, as gtk3 will give
# weird rendering artifacts in conjunction with a remote X11 display like XQuartz 
# on Mac
RUN ./autogen.sh && \
    ./configure \
        --prefix=/usr \
        --sysconfdir=/etc \
        --localstatedir=/var \
        --sharedstatedir=/var/lib \
        --libexecdir=/usr/lib \
        --localstatedir=/var/lib \
        --infodir=/usr/share/info \
        --mandir=/usr/share/man \
        --with-modules \
        --with-file-notification=inotify \
        --with-mailutils \
        --with-harfbuzz \
        --with-json \
        --with-x=yes \
        --with-xft \
        --with-x-toolkit=lucid \
        --with-lcms2 \
        --with-cairo \
        --with-xpm=yes \
        --with-gif=yes \
        --with-gnutls=yes \
        --with-jpeg=yes \
        --with-png=yes \
        --with-tiff=yes \
        --with-rsvg \
        CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security" \
        CPPFLAGS="-Wdate-time -D_FORTIFY_SOURCE=2" LDFLAGS="-Wl,-Bsymbolic-functions -Wl,-z,relro" && \
    make && \
    checkinstall --install=no --default --pkgname=emacs --pkgversion="${EMACS_VERSION}" && \
    cp emacs*.deb /emacs.deb

# Create installer for latest org version
RUN mkdir -p /tmp/org/src && \
    cd /tmp/org/src && \
    git clone https://git.savannah.gnu.org/git/emacs/org-mode.git && \
    cd org-mode \
    && make autoloads \
    && make \
    && \
    checkinstall --install=no --default --pkgname=emacs-org --pkgversion="9.5" && \
    cp emacs-org*.deb /emacs-org.deb

# Get the citation-style-language styles, so we can use them with the new org-mode
RUN git clone https://github.com/citation-style-language/styles /tmp/csl/styles && \
    git clone https://github.com/citation-style-language/locales /tmp/csl/locales

FROM authsec/sphinx:1.0.7

ENV EMIAC_USER=emiac
ENV EMIAC_GROUP=dialout
ENV EMIAC_HOME=/home/${EMIAC_USER}

COPY --from=build /emacs* /tmp
RUN dpkg -i /tmp/emacs.deb && \
    # We force overwrite here, as we do want the new org-version to 
    # overwrite the old one
    dpkg -i --force-overwrite /tmp/emacs-org.deb && \
    rm /tmp/emacs*.deb

# Install csl styles and locales, so `#+cite_export: csl` works
COPY --from=build /tmp/csl/styles/*.csl /usr/share/emacs/${EMACS_VERSION}/etc/org/csl/
COPY --from=build /tmp/csl/locales/*.xml /usr/share/emacs/${EMACS_VERSION}/etc/org/csl/
COPY --from=build /tmp/csl/locales/locales.json /usr/share/emacs/${EMACS_VERSION}/etc/org/csl/

COPY fonts/* /tmp/fonts/

WORKDIR /usr/share/fonts/truetype/
RUN unzip /tmp/fonts/Roboto_Mono.zip -d /usr/share/fonts/truetype/roboto-mono/

RUN mkdir roboto-mono-nerd && \
    cd roboto-mono-nerd && \
    curl -o "Roboto Mono Nerd Font.ttf" https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/RobotoMono/Regular/complete/Roboto%20Mono%20Nerd%20Font%20Complete.ttf
RUN fc-cache -f

# Now create a nice user and group that we can use to base our config on
# the user id 501 and group id 20 (dialout) map to the group ids used on 
# a MacOS default user.
RUN useradd -rm -d ${EMIAC_HOME} -s /bin/bash -g ${EMIAC_GROUP} -u 501 ${EMIAC_USER}

# Theoretically we can mount this location from the outside too and therefore 
# use what we have on the host OS side.
WORKDIR ${EMIAC_HOME}/.emacs.d/
COPY config/init.el .
RUN chown -R ${EMIAC_USER} ${EMIAC_HOME} && \
    chmod 0755 ${EMIAC_HOME}/.emacs.d/init.el

# Create a configuration folder for emiac where custom scripts can be
# stored for execution before starting emacs, so e.g. the git configuration 
# can be run before starting emacs.
# This is typically a mount point for a configuration coming from a user
RUN mkdir ${EMIAC_HOME}/.emiac

# Run emacs as user in this container
USER ${EMIAC_USER}

# Download the configuration into the container by starting emacs.
# As this might throw errors, we signal ok with the last echo command
RUN emacs --daemon --eval "(kill-emacs)"; echo "Signal OK"

USER root

# install pdf-tools dependencies, so the server does not have to be built on 
# initial startup of emiac
RUN apt update && apt install -y elpa-pdf-tools-server firefox imagemagick

# Create bin folder where we can put our custom emiac shell script
RUN mkdir ${EMIAC_HOME}/bin 
COPY emiac.sh ${EMIAC_HOME}/bin
RUN chmod -R 0755 ${EMIAC_HOME} && chown -R ${EMIAC_USER}:${EMIAC_GROUP} ${EMIAC_HOME}

# Setup turnkey SSH keys. If the user puts `emiac_ssh_key.pub` into his `.ssh` folder
# emiac can issue commands (initially to an MacOS shell) to open URLs and other required 
# programs that should be reached from the guest OS on the host OS.
COPY .ssh/ ${EMIAC_HOME}/.ssh
RUN chown -R ${EMIAC_USER}:${EMIAC_GROUP} ${EMIAC_HOME}/.ssh && chmod 0700 ${EMIAC_HOME}/.ssh

# Install Binary Hugo Builds
# from https://github.com/hugoguru/dist-hugo/releases
WORKDIR /usr/local/bin
RUN curl -L https://github.com/hugoguru/dist-hugo/releases/download/v0.91.2/hugo-extended-0.91.2-linux-$(uname -m).tar.gz | tar xz 

# Run emacs as user in this container
USER ${EMIAC_USER}

# We'll map our work environment into this folder by default from the outside
# so we have persistence of our work later
WORKDIR ${EMIAC_HOME}/research

CMD ["/home/emiac/bin/emiac.sh"]
