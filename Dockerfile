FROM authsec/sphinx:1.0.7

WORKDIR /tmp
# Install build dependencies
RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt -y install build-essential libgtk-3-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev libwebkit2gtk-4.0-dev libgnutls28-dev autoconf libxft-dev libxaw7-dev
# Download emacs 
RUN curl https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz | tar xz && \
    mv emacs* emacs

WORKDIR /tmp/emacs

# Create emacs installer
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
        CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security" \
        CPPFLAGS="-Wdate-time -D_FORTIFY_SOURCE=2" LDFLAGS="-Wl,-Bsymbolic-functions -Wl,-z,relro" && \
    make && \
    make install

# Create installer for latest org version
RUN mkdir -p /tmp/org/src && \
    cd /tmp/org/src && \
    git clone https://git.savannah.gnu.org/git/emacs/org-mode.git && \
    cd org-mode \
    && make autoloads \
    && make \
    && make install

COPY fonts/* /tmp/fonts/

WORKDIR /usr/share/fonts/truetype/
RUN unzip /tmp/fonts/Roboto_Mono.zip -d /usr/share/fonts/truetype/roboto-mono/

RUN mkdir roboto-mono-nerd && \
    cd roboto-mono-nerd && \
    curl -o "Roboto Mono Nerd Font.ttf" https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/RobotoMono/Regular/complete/Roboto%20Mono%20Nerd%20Font%20Complete.ttf
RUN fc-cache -f

# Now create a nice user that we can use to base our config on
RUN useradd -rm -d /home/emiac -s /bin/bash -g root -G sudo -u 1001 emiac



# Theoretically we can mount this location from the outside too and therefore 
# use what we have on the host OS side.
WORKDIR /home/emiac/.emacs.d/
COPY config/init.el .
RUN chown -R emiac /home/emiac && \
    chmod 0755 /home/emiac/.emacs.d/init.el

# Run emacs as user in this container
USER emiac
# Download the configuration into the container by starting emacs.
# As this might throw errors, we signal ok with the last echo command
RUN emacs --daemon --eval "(kill-emacs)"; echo "Signal OK"


# We'll map our work environment into this folder from the outside
# so we have persistence later
WORKDIR /home/emiac/research

CMD ["emacs"]
