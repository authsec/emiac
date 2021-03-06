ARG ORG_VERSION="9.5.2"
ARG EMACS_VERSION="27.2"

FROM ubuntu:focal AS build

ARG EMACS_VERSION
ENV EMACS_VERSION=$EMACS_VERSION

# Version number as found after the tag e.g release_${ORG_VERSION}
ARG ORG_VERSION
ENV ORG_VERSION=${ORG_VERSION}

WORKDIR /tmp

ARG DEBIAN_FRONTEND=noninteractive
# Install build dependencies (can also be used to build with gtk3 instead of the [here preferred] lucid toolkit)
RUN apt update && apt -y upgrade && \
    apt -y install \
        curl \
        checkinstall \
        coreutils \
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
        librsvg2-dev \
        ruby-dev

WORKDIR /tmp

# Download emacs 
RUN curl "https://ftp.gnu.org/pub/gnu/emacs/emacs-${EMACS_VERSION}.tar.gz" | tar xz && \
    mv emacs* emacs

WORKDIR /tmp/emacs

# Create emacs installer and make sure to use x-toolkit lucid, as gtk3 will give
# weird rendering artifacts in conjunction with a remote X11 display like XQuartz 
# on Mac
RUN ./autogen.sh && \
    ./configure \
        --prefix=/home/emiac/local \
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
    checkinstall --install=yes --default --pkgname=emacs --pkgversion="${EMACS_VERSION}" && \
    cp emacs*.deb /emacs.deb

# Create installer for latest org version, run this before compiling emacs from scratch,
# as realpath might not find the emacs binary otherwise. This issue seems to happen when
# building on github only.
ENV PATH=/home/emiac/local/bin:${PATH}
WORKDIR /tmp
#RUN curl https://git.savannah.gnu.org/cgit/emacs/org-mode.git/snapshot/org-mode-release_${ORG_VERSION}.tar.gz | tar xz && \
#    mv org-mode* org-mode
RUN git clone https://git.savannah.gnu.org/git/emacs/org-mode.git
WORKDIR /tmp/org-mode
RUN git checkout release_${ORG_VERSION}
COPY local.mk .
RUN make autoloads
RUN make
RUN mkdir /emiac && make DESTDIR=/emiac install
#RUN checkinstall -d 2 --install=no --default --pkgname=emacs-org --pkgversion=${ORG_VERSION}
RUN gem install fpm
RUN ls -la /emiac
RUN fpm -s dir -t deb -C /emiac --name EmIAC --version 1.0.0 --iteration 1 --description "EMIAC" .
RUN ls -la
RUN cp emiac*.deb /emacs-org.deb

# Get the citation-style-language styles, so we can use them with the new org-mode
RUN git clone https://github.com/citation-style-language/styles /tmp/csl/styles && \
    git clone https://github.com/citation-style-language/locales /tmp/csl/locales

# Clone authsec styles into the image
RUN git clone https://github.com/authsec/latex-styles.git /tmp/latex-styles

# Clone all the icons into font directory, so they do not have to be downloaded
RUN git clone https://github.com/domtronn/all-the-icons.el.git /tmp/all-the-icons

FROM authsec/sphinx:latest
LABEL maintainer="Jens Frey <jens.frey@coffeecrew.org>" Version="2022-01-29"

ARG EMACS_VERSION
ARG ORG_VERSION

# Setup environment used in docker build and scripts
# running in the container itself.
ENV EMIAC_USER=emiac
ENV EMIAC_GROUP=dialout
ENV EMIAC_HOME=/home/${EMIAC_USER}
ENV EMIAC_BIN_DIR=${EMIAC_HOME}/bin
ENV EMIAC_CONFIG_DIR=${EMIAC_HOME}/.emiac
ENV EMIAC_CONFIG_DEFAULTS_DIR=${EMIAC_CONFIG_DIR}/defaults
ENV EMIAC_INIT_FILE=${EMIAC_CONFIG_DIR}/emiac.el
ENV EMIAC_USER_INIT_DIR=${EMIAC_HOME}/.emacs.d
ENV EMACS_VERSION=$EMACS_VERSION
ENV EMIAC_RESEARCH_DIR=${EMIAC_HOME}/research
# If set to 1 the user configuration should be taken from inside .emacs.d
# of the mounted research folder.
ENV EMIAC_EXTERNALIZE_CONFIGURATION=${EMIAC_EXTERNALIZE_CONFIGURATION:-0}
ENV HUGO_VERSION=0.92.0
ENV ORG_VERSION=${ORG_VERSION}
    
COPY --from=build /emacs* /tmp
RUN dpkg -i /tmp/emacs.deb && \
    # We force overwrite here, as we do want the new org-version to 
    # overwrite the old one
    dpkg -i --force-overwrite /tmp/emacs-org.deb 

# Install csl styles and locales, so `#+cite_export: csl` works
COPY --from=build /tmp/csl/styles/*.csl /home/emiac/local/share/emacs/${EMACS_VERSION}/etc/org/csl/
COPY --from=build /tmp/csl/locales/*.xml /home/emiac/local/share/emacs/${EMACS_VERSION}/etc/org/csl/
COPY --from=build /tmp/csl/locales/locales.json /home/emiac/local/share/emacs/${EMACS_VERSION}/etc/org/csl/

# Install style directly into image. This can be overridden/extended by mounting an additional style
# file directory into the container on /home/emiac/texmf/tex/latex/commonstuff
COPY --from=build /tmp/latex-styles/*.sty /usr/share/texlive/texmf-dist/tex/latex/authsec/
RUN texhash

# Setup Fonts
# The all the icons package installs to this location
COPY --from=build /tmp/all-the-icons/fonts/*.ttf /home/emiac/.local/share/fonts/
COPY fonts/* /tmp/fonts/

WORKDIR /usr/share/fonts/truetype/
RUN unzip /tmp/fonts/Roboto_Mono.zip -d /usr/share/fonts/truetype/roboto-mono/
RUN unzip /tmp/fonts/Fira_Code.zip -d /usr/share/fonts/truetype/fira-code/

RUN mkdir roboto-mono-nerd && \
    cd roboto-mono-nerd && \
    curl -o "Roboto Mono Nerd Font.ttf" https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/RobotoMono/Regular/complete/Roboto%20Mono%20Nerd%20Font%20Complete.ttf
RUN fc-cache -f

# Now create a nice user and group that we can use to base our config on
# the user id 501 and group id 20 (dialout) map to the group ids used on 
# a MacOS default user.
RUN useradd -rm -d ${EMIAC_HOME} -s /bin/bash -g ${EMIAC_GROUP} -u 501 ${EMIAC_USER}

# Create bin folder where we can put our custom emiac shell script
# Create research folder where the user stuff will end up
RUN mkdir ${EMIAC_HOME}/bin ${EMIAC_RESEARCH_DIR}
COPY emiac.sh create_scaffolding ${EMIAC_HOME}/bin
RUN chmod -R 0755 ${EMIAC_HOME} && chown -R ${EMIAC_USER}:${EMIAC_GROUP} ${EMIAC_HOME}

# Theoretically we can mount this location from the outside too and therefore 
# use what we have on the host OS side.
WORKDIR ${EMIAC_HOME}/.emacs.d/
# Copy to a different name here, so emacs doesn't find a expected file here by default
COPY config/defaults/init.el ./emiac-init.el
RUN chown -R ${EMIAC_USER} ${EMIAC_HOME} 

# Create a configuration folder for emiac where custom scripts can be
# stored for execution before starting emacs, so e.g. the git configuration 
# can be run before starting emacs.
# This is typically a mount point for a configuration coming from a user
COPY config/emiac.el ${EMIAC_CONFIG_DIR}/
# Copy default configurations
COPY config/defaults/ ${EMIAC_CONFIG_DEFAULTS_DIR}/
RUN chmod -R 0755 ${EMIAC_HOME} && chown -R ${EMIAC_USER}:${EMIAC_GROUP} ${EMIAC_HOME}

# Run emacs as user in this container
USER ${EMIAC_USER}

# Download the full configuration into the container by starting emacs.
# As this might throw errors, we signal ok with the last echo command
RUN ${EMIAC_HOME}/local/bin/emacs -q --load ${EMIAC_INIT_FILE} --daemon --eval "(emacsql-sqlite-compile) (kill-emacs)"; echo "Signal OK"


USER root

# install pdf-tools dependencies, so the server does not have to be built on 
# initial startup of emiac
RUN apt update && apt install -y elpa-pdf-tools-server imagemagick

# Setup turnkey SSH keys. If the user puts `emiac_ssh_key.pub` into his `.ssh` folder
# emiac can issue commands (initially to an MacOS shell) to open URLs and other required 
# programs that should be reached from the guest OS on the host OS.
COPY .ssh/ ${EMIAC_HOME}/.ssh
RUN chown -R ${EMIAC_USER}:${EMIAC_GROUP} ${EMIAC_HOME}/.ssh && chmod 0700 ${EMIAC_HOME}/.ssh

# Install Binary Hugo Builds
# from https://github.com/hugoguru/dist-hugo/releases
WORKDIR /usr/local/bin
RUN curl -L https://github.com/hugoguru/dist-hugo/releases/download/v${HUGO_VERSION}/hugo-extended-${HUGO_VERSION}-linux-$(uname -m).tar.gz | tar xz 

# Cleanup
RUN rm -rf /tmp/* && \
    apt-get autoremove -y && \
    apt-get clean && \
    find /var/lib/apt/lists -type f | xargs rm && \
    find /var/log -type f -exec rm {} \; && \
    rm -rf /usr/share/man/* && \
    rm -rf /usr/share/doc/* && \
    rm -f /var/log/alternatives.log /var/log/apt/* && \
    rm -f /var/cache/debconf/*-old

# Run emacs as user in this container
USER ${EMIAC_USER}

# We'll map our work environment into this folder by default from the outside
# so we have persistence of our work later
WORKDIR ${EMIAC_RESEARCH_DIR}

CMD ["/home/emiac/bin/emiac.sh"]
