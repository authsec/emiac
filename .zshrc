# Example Function to put into your .zshrc to properly set up EmIAC with all the features.
emiac() {
    HOST_IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
    HOST_USER=${USER}
    xhost +${HOST_IP}
    docker run -d -it --rm \
           -e DISPLAY=${HOST_IP}:0 \
           -e HOST_IP=${HOST_IP} \
           -e HOST_USER=${HOST_USER} \
           -p1313:1313 \
           -v ~/research:/home/emiac/research:rw \
           -v ~/coding/github/latex-styles:/home/emiac/texmf/tex/latex/commonstuff:rw \
           -v ~/research/emiac_ssh_key:/home/emiac/.ssh/id_rsa authsec/emiac
}

# Example Function to test new integrated container
emiac-dev() {
    HOST_IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
    HOST_USER=${USER}
    xhost +${HOST_IP}
    docker run -d -it --rm \
           -e DISPLAY=${HOST_IP}:0 \
           -e HOST_IP=${HOST_IP} \
           -e HOST_USER=${HOST_USER} \
           -p1314:1313 \
           -v ~/emiac/re1:/home/emiac/research:rw \
           authsec/emiac:dev
}

# Example Function to test configuration externalization
emiac-dev2() {
    HOST_IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
    HOST_USER=${USER}
    xhost +${HOST_IP}
    docker run -d -it --rm --name emiac-dev2 \
           -e DISPLAY=${HOST_IP}:0 \
           -e HOST_IP=${HOST_IP} \
           -e HOST_USER=${HOST_USER} \
           -e EMIAC_EXTERNALIZE_CONFIGURATION=1 \
           -p1315:1313 \
           -v ~/emiac/re2:/home/emiac/research:rw \
           authsec/emiac:dev
}

# Required for finder to open files.
emiac-dev3() {
    HOST_IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
    HOST_USER=${USER}
    HOST_RESEARCH_DIR="${HOME}/emiac/re3"
    xhost +${HOST_IP}
    docker run -d -it --rm --name emiac-dev3 \
           -e DISPLAY=${HOST_IP}:0 \
           -e HOST_IP=${HOST_IP} \
           -e HOST_USER=${HOST_USER} \
           -e HOST_RESEARCH_DIR=${HOST_RESEARCH_DIR} \
           -e EMIAC_EXTERNALIZE_CONFIGURATION=1 \
           -p1316:1313 \
           -v ${HOST_RESEARCH_FOLDER}:/home/emiac/research:rw \
           authsec/emiac:dev
}