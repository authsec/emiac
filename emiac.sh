#!/bin/bash

# ENVIRONMENT VARIABLES PROVIDED BY DOCKER IMAGE

if [ "${EMIAC_EXTERNALIZE_CONFIGURATION}" == "1" ]
then
    echo "Setting up with externalized configuration"
    export EMIAC_USER_INIT_DIR="${EMIAC_RESEARCH_DIR}/.emacs.d"
    export EMIAC_CONFIG_DIR="${EMIAC_RESEARCH_DIR}/.emiac"
fi

# Initialize directory structure if required
${EMIAC_BIN_DIR}/create_scaffolding



if [ -f "${EMIAC_CONFIG_DIR}/setup" ]
then
cat <<'EOF'
[EMIAC INFO]
    Running custom setup file ...

    Make sure the file has a supported interpreter set in the first line.

    This is typically

    #!/bin/bash

    But can also refer to e.g. a python interpreter like

    #!/usr/bin/python3

[/EMIAC INFO]

EOF
    echo "[EMIAC] Ensuring we can execute requested custom setup"
    chmod 0755 ${EMIAC_CONFIG_DIR}/setup

    echo -en "[EMIAC] Running custom setup\n\n\n"
    ${EMIAC_CONFIG_DIR}/setup
    
else
cat <<EOF
[EMIAC INFO]
If you want to customize your emiac installation
you can create a 'setup' file in ${EMIAC_CONFIG_DIR}
and mount that file or folder into the container.

EOF
cat <<'EOF'
Make sure you mount the file properly and make sure you have 
allowed the container runtime access to that folder e.g.

#> colima start --mount $HOME/research:w --mount $HOME/emiac_config_DIREMIAC_CONFIG_DIR:w
You can then mount the folder with the run command like so:

#> docker run -it --rm -e DISPLAY=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'):0 -v ~/research:/home/emiac/research:rw -v ~/emiac_config_DIREMIAC_CONFIG_DIR/setup:/home/emiac/.emiac/setup:rw authsec/emiac

If you DO want emiac to communicate with the host OS through SSH to e.g. open URLs or folders for you, you need to cat the `emiac_ssh_key.pub` to your ~/.ssh/authorized_keys file.

E.g. on your mac `cat emiac_ssh_key.pub >> ~/.ssh/authorized_keys`

The script will be executed before 'emacs'.
[/EMIAC INFO]

EOF
# EOF Needs to be kept left, don't indent as the if condition will
# no longer work!!
fi

if [ ! -f "${EMIAC_USER_INIT_DIR}/init.el" ]
then
    # Make sure directory is present
    mkdir -p ${EMIAC_USER_INIT_DIR}
    echo " Missing user configuration, copying one from default"
    cp ${EMIAC_CONFIG_DEFAULTS_DIR}/init.el ${EMIAC_USER_INIT_DIR}
fi

echo "[EMIAC] Starting emacs ..."
emacs -q --load ${EMIAC_INIT_FILE} -T "EmIAC - Research Environment"
