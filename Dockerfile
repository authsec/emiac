FROM authsec/sphinx:1.0.7

#RUN apt update && apt -y install software-properties-common && add-apt-repository -y ppa:kelleyk/emacs
RUN apt update && apt -y install fonts-roboto #emacs27
RUN fc-cache -f

WORKDIR /tmp
RUN apt -y install build-essential libgtk-3-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev libwebkit2gtk-4.0-dev libgnutls28-dev autoconf
RUN curl https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz | tar xz
RUN mv emacs* emacs
WORKDIR /tmp/emacs
RUN ls -la 
RUN ./autogen.sh && ./configure --prefix=/usr --sysconfdir=/etc --localstatedir=/var --sharedstatedir=/var/lib --libexecdir=/usr/lib --localstatedir=/var/lib --infodir=/usr/share/info --mandir=/usr/share/man --with-modules --with-file-notification=inotify --with-mailutils --with-harfbuzz --with-json --with-x=yes --with-x-toolkit=gtk3 --with-lcms2 --with-cairo --with-xpm=yes --with-gif=yes --with-gnutls=yes --with-jpeg=yes --with-png=yes --with-tiff=yes --with-xwidgets CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security" CPPFLAGS="-Wdate-time -D_FORTIFY_SOURCE=2" LDFLAGS="-Wl,-Bsymbolic-functions -Wl,-z,relro"
RUN make && make install

RUN mkdir ~/.emacs.d/ 
COPY config/init.el /root/.emacs.d/
COPY fonts/* /tmp/fonts/
RUN unzip /tmp/fonts/Roboto_Mono.zip -d /usr/share/fonts/truetype/roboto-mono/

RUN emacs --daemon --eval "(kill-emacs)"; echo "Signal OK"

CMD ["emacs"]
