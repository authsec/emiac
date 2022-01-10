# Example Function to put into your .zshrc to properly set up EmIAC with all the features.
emiac() {
    HOST_IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
    HOST_USER=${USER}
    docker run -d -it --rm \
           -e DISPLAY=${HOST_IP}:0 \
           -e HOST_IP=${HOST_IP} \
           -e HOST_USER=${HOST_USER} \
           -p1313:1313 \
           -v ~/research:/home/emiac/research:rw \
           -v ~/coding/github/latex-styles:/home/emiac/texmf/tex/latex/commonstuff:rw \
           -v ~/research/emiac_ssh_key:/home/emiac/.ssh/id_rsa authsec/emiac
}